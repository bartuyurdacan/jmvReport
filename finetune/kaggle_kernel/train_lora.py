# jmvReport interpretation model: LoRA fine-tune of Qwen3-1.7B on synthetic (results digest -> interpretation) pairs
import json, os, random, subprocess, sys, torch
from datasets import Dataset
from transformers import AutoTokenizer, AutoModelForCausalLM, TrainingArguments, Trainer
from peft import LoraConfig, get_peft_model

BASE = os.environ.get("BASE_MODEL", "Qwen/Qwen3-1.7B")
DATA = "/kaggle/input/jmvreport-train/train_en.jsonl"
OUT = "/kaggle/working"
MAXLEN = 1024
random.seed(0)

rows = [json.loads(l) for l in open(DATA, encoding="utf-8")]
random.shuffle(rows)
n_eval = max(20, len(rows) // 20)
eval_rows, train_rows = rows[:n_eval], rows[n_eval:]
print("train", len(train_rows), "eval", len(eval_rows))

tok = AutoTokenizer.from_pretrained(BASE)
tok.pad_token = tok.eos_token if tok.pad_token is None else tok.pad_token

def build(r):
    msgs = [{"role": "system", "content": r["system"]}, {"role": "user", "content": r["user"]}]
    prompt = tok.apply_chat_template(msgs, tokenize=False, add_generation_prompt=True, enable_thinking=False)
    full = prompt + r["assistant"] + tok.eos_token
    p_ids = tok(prompt, add_special_tokens=False)["input_ids"]
    f_ids = tok(full, add_special_tokens=False)["input_ids"][:MAXLEN]
    labels = [-100] * min(len(p_ids), len(f_ids)) + f_ids[len(p_ids):]
    return {"input_ids": f_ids, "labels": labels}

train_ds = Dataset.from_list([build(r) for r in train_rows])
eval_ds = Dataset.from_list([build(r) for r in eval_rows])

def collate(batch):
    m = max(len(b["input_ids"]) for b in batch)
    ids = torch.full((len(batch), m), tok.pad_token_id); lab = torch.full((len(batch), m), -100); att = torch.zeros((len(batch), m), dtype=torch.long)
    for i, b in enumerate(batch):
        n = len(b["input_ids"]); ids[i, :n] = torch.tensor(b["input_ids"]); lab[i, :n] = torch.tensor(b["labels"]); att[i, :n] = 1
    return {"input_ids": ids, "labels": lab, "attention_mask": att}

model = AutoModelForCausalLM.from_pretrained(BASE, torch_dtype=torch.float16, device_map={"": 0})
model.gradient_checkpointing_enable(); model.enable_input_require_grads()
model = get_peft_model(model, LoraConfig(r=16, lora_alpha=32, lora_dropout=0.05, task_type="CAUSAL_LM",
                                         target_modules=["q_proj", "k_proj", "v_proj", "o_proj", "gate_proj", "up_proj", "down_proj"]))
model.print_trainable_parameters()

args = TrainingArguments(output_dir=f"{OUT}/ckpt", per_device_train_batch_size=4, gradient_accumulation_steps=4, num_train_epochs=2,
                         learning_rate=2e-4, lr_scheduler_type="cosine", warmup_ratio=0.03, logging_steps=20, save_strategy="no",
                         eval_strategy="steps", eval_steps=100, fp16=True, report_to=[], dataloader_num_workers=2, optim="adamw_torch")
Trainer(model=model, args=args, train_dataset=train_ds, eval_dataset=eval_ds, data_collator=collate).train()

model.save_pretrained(f"{OUT}/lora_adapter")
merged = model.merge_and_unload(); merged.save_pretrained(f"{OUT}/merged", safe_serialization=True); tok.save_pretrained(f"{OUT}/merged")

# quick generation check
merged.eval()
for r in eval_rows[:4]:
    msgs = [{"role": "system", "content": r["system"]}, {"role": "user", "content": r["user"]}]
    ids = tok.apply_chat_template(msgs, add_generation_prompt=True, enable_thinking=False, return_tensors="pt").to(merged.device)
    with torch.no_grad(): out = merged.generate(ids, max_new_tokens=220, do_sample=False)
    print("\n=== TYPE", r["type"]); print("REF:", r["assistant"]); print("GEN:", tok.decode(out[0][ids.shape[1]:], skip_special_tokens=True))

# convert to GGUF (q8_0)
subprocess.run("git clone --depth 1 https://github.com/ggml-org/llama.cpp /kaggle/working/llama.cpp && pip -q install -r /kaggle/working/llama.cpp/requirements/requirements-convert_hf_to_gguf.txt", shell=True, check=True)
subprocess.run(f"python /kaggle/working/llama.cpp/convert_hf_to_gguf.py {OUT}/merged --outtype q8_0 --outfile {OUT}/jmvreport-en-q8_0.gguf", shell=True, check=True)
subprocess.run(f"rm -rf {OUT}/llama.cpp {OUT}/ckpt {OUT}/merged && ls -la {OUT}", shell=True)
print("DONE")

test_that("fidelity check accepts faithful rewrites and rejects new numbers", {
  ref <- "<p>Gruplar arasında fark bulunmamıştır, t(58) = 1.92, p = .060, d = 0.49.</p>"
  ok <- "<p>Bağımsız örneklemler t-testi gruplar arasında anlamlı bir fark göstermemiştir, t(58) = 1.92, p = .060, d = 0.49.</p>"
  bad <- "<p>Gruplar arasında fark bulunmamıştır, t(58) = 1.93, p = .060, d = 0.49.</p>"
  expect_true(check_fidelity(ok, ref)$ok)
  expect_false(check_fidelity(bad, ref)$ok)
  dropped <- "<p>Gruplar arasında fark bulunmamıştır, t(58) = 1.92.</p>"
  expect_false(check_fidelity(dropped, ref)$ok)
})

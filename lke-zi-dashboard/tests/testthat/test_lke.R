# Jalankan dari folder lke-zi-dashboard:  Rscript -e 'testthat::test_dir("tests/testthat")'
# Tes ini hanya memakai rubrik publik (data/rubrik_lke.csv) dan data buatan.

akar <- normalizePath(file.path(testthat::test_path(), "..", ".."))
lama <- setwd(akar)
for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f, encoding = "UTF-8")
RB <- muat_rubrik()
setwd(lama)

isi_semua <- function(nilai_kategori = "A") {
  bt <- RB[RB$tipe == "butir", ]
  j <- stats::setNames(rep("", nrow(RB)), RB$kode)
  for (i in seq_len(nrow(bt))) {
    p <- bt$pilihan[i]; k <- bt$kode[i]
    j[[k]] <- if (p == "Ya/Tidak") (if (nilai_kategori == "A") "Ya" else "Tidak")
              else if (p %in% names(PETA_NILAI)) nilai_kategori
              else if (p == "Nilai (0-4)") "4" else ""
  }
  j[RB$kode[RB$tipe == "rincian"]] <- "10"
  # penurunan pelanggaran: tahun lalu 10, tahun ini 0, disanksi 0 -> 100%
  turun <- RB$kode[RB$tipe == "butir" & RB$rumus_persen %in% "penurunan"]
  rc <- RB$kode[RB$induk %in% turun]
  j[rc] <- c("10", "0", "0")
  j
}

test_that("rubrik memuat 113 butir dengan total bobot Pengungkit 60 dan Hasil 40", {
  expect_equal(sum(RB$tipe == "butir"), 113)
  kp <- unique(RB[RB$bagian == "Pengungkit", c("area_kode", "komponen_no", "bobot_komponen")])
  expect_equal(sum(kp$bobot_komponen), 60)
  hs <- RB[RB$bagian == "Hasil" & RB$tipe == "butir", ]
  expect_equal(sum(hs$bobot_butir), 40)
  expect_false(anyDuplicated(RB$kode) > 0)
})

test_that("bobot sub-komponen menjumlah ke bobot komponen", {
  s <- unique(RB[RB$bagian == "Pengungkit", c("sub_kode", "area_kode", "komponen_no", "bobot_sub", "bobot_komponen")])
  a <- stats::aggregate(bobot_sub ~ area_kode + komponen_no + bobot_komponen, s, sum)
  expect_equal(a$bobot_sub, a$bobot_komponen)
})

test_that("jawaban sempurna menghasilkan nilai 100", {
  s <- hitung_skor(RB, isi_semua("A"))$ringkas
  expect_equal(s$skor[s$bagian == "Total"], 100, tolerance = 1e-9)
})

test_that("peta nilai sama dengan rumus Excel", {
  expect_equal(nilai_pilihan("A/B/C/D", "B"), 0.67)
  expect_equal(nilai_pilihan("A/B/C/D/E", "D"), 0.25)
  expect_equal(nilai_pilihan("A/B/C", "B"), 0.5)
  expect_true(is.na(nilai_pilihan("A/B/C", "")))
})

test_that("butir kosong dihitung 0 secara bawaan, diabaikan bila meniru Excel", {
  j <- isi_semua("A"); j[["P1.i.b"]] <- ""
  konservatif <- hitung_skor(RB, j)$sub
  excel <- hitung_skor(RB, j, kosong_nol = FALSE)$sub
  expect_equal(konservatif$skor[konservatif$sub_kode == "P1.i"], 0.25)
  expect_equal(excel$skor[excel$sub_kode == "P1.i"], 0.5)
})

test_that("persentase: rasio dibatasi 100%, penyebut 0 terdeteksi, penurunan pelanggaran", {
  expect_equal(hitung_persen("rasio", c(4, 2))$nilai, 0.5)
  expect_equal(hitung_persen("rasio", c(4, 9))$nilai, 1)
  expect_true(is.na(hitung_persen("rasio", c(0, 0))$nilai))
  expect_equal(hitung_persen("penurunan", c(0, 0, 0))$nilai, 1)
  expect_equal(hitung_persen("penurunan", c(4, 1, 1))$nilai, 0.75)
  expect_equal(hitung_persen("penurunan", c(1, 3, 0))$nilai, 0)
})

test_that("kriteria A/B/C dipecah per huruf", {
  b <- RB[RB$kode == "P1.ii.c", ]
  k <- kriteria_per_pilihan(b$penjelasan, b$pilihan)
  expect_named(k, c("A", "B", "C"))
  expect_match(k[["A"]], "berkala")
  expect_match(k[["B"]], "tidak secara berkala")
})

test_that("validasi menandai narasi kosong, bukti tanpa tautan, dan klaim berkala tanpa bukti", {
  b <- RB[RB$kode == "P1.ii.c", ]
  tm <- validasi_butir(b, RB[0, ], list(jawaban = "A", narasi = "", bukti = "Laporan sosialisasi 2026"), tahun = 2026)
  expect_true(all(c("V10", "V12", "V20") %in% tm$aturan))
  narasi <- strrep("Sosialisasi dilakukan setiap triwulan secara berkala melalui website dan rapat. ", 5)
  tm2 <- validasi_butir(b, RB[0, ], list(jawaban = "A", narasi = narasi,
                                          bukti = "2026_Laporan Sosialisasi ZI Triwulan I-IV | https://contoh.id/a.pdf"), tahun = 2026)
  expect_equal(nrow(tm2[tm2$tingkat %in% c("merah", "kuning"), ]), 0)
})

test_that("negasi 'tidak secara berkala' tidak dianggap syarat", {
  expect_false("berkala" %in% konsep_dalam_kriteria("Jika dilakukan secara terbatas dan tidak secara berkala"))
  expect_true("berkala" %in% konsep_dalam_kriteria("dilakukan secara berkala"))
})

test_that("validasi menangkap #DIV/0 dan rincian LHKPN yang tidak cocok", {
  b <- RB[RB$kode == "R5.iii.1.1", ]; rc <- RB[RB$induk %in% b$kode, ]
  v <- stats::setNames(c("10", "1", "3", "5", "8"), rc$kode)       # 1+3+5 != 10
  tm <- validasi_butir(b, rc, list(jawaban = "", narasi = "", bukti = ""), v, tahun = 2026)
  expect_true("V05" %in% tm$aturan)
  b2 <- RB[RB$kode == "R5.ii.1", ]; rc2 <- RB[RB$induk %in% b2$kode, ]
  tm2 <- validasi_butir(b2, rc2, list(jawaban = "", narasi = "", bukti = ""), stats::setNames(c("0", "0", "0"), rc2$kode), tahun = 2026)
  expect_true("V03" %in% tm2$aturan)
})

test_that("konsistensi antarbutir: jumlah perubahan agen perubahan harus sama", {
  isian <- data.frame(kode = c("R1.i.a.s1", "R1.i.a.s2", "R1.i.b.s1", "R1.i.b.s2"),
                      jawaban = c("4", "2", "3", "3"), narasi = "", bukti = "", stringsAsFactors = FALSE)
  tm <- validasi_semua(RB, isian, NULL, 2026)
  expect_true(any(tm$aturan == "V07"))
})

test_that("bukti dipasangkan dari kolom bernomor Excel", {
  x <- pasangkan_bukti("1. SK Tim 2026\n\n2. Notulen 2026", "1. https://a\n\n2. https://b")
  expect_equal(x, "SK Tim 2026 | https://a\nNotulen 2026 | https://b")
})

test_that("panggilan Claude diproses dengan benar (respons tiruan)", {
  Sys.setenv(ANTHROPIC_API_KEY = "kunci-uji")
  on.exit(Sys.unsetenv("ANTHROPIC_API_KEY"))
  tiruan <- function(req) {
    body <- req$body$data
    expect_equal(body$model, KONFIG$model)
    expect_equal(body$fallbacks, "default")
    expect_equal(body$output_config$format$type, "json_schema")
    httr2::response_json(body = list(
      model = KONFIG$model, stop_reason = "end_turn",
      usage = list(input_tokens = 1000, output_tokens = 200),
      content = list(list(type = "thinking", thinking = ""),
                     list(type = "text", text = jsonlite::toJSON(list(
                       jawaban_disarankan = "B", tingkat_risiko = "tinggi", tingkat_keyakinan = "sedang", alasan = "Tidak berkala",
                       unsur_kriteria = list(list(unsur = "berkala", status = "tidak_terbukti", dasar_bukti = "Laporan TW I", keterangan = "1 periode")),
                       dokumen_perlu_ditambah = list("Laporan sosialisasi TW II-IV"),
                       perbaikan_narasi = "Sebutkan jadwal"), auto_unbox = TRUE)))))
  }
  b <- RB[RB$kode == "P1.ii.c", ]
  h <- httr2::with_mocked_responses(tiruan, claude_validasi_butir(b, list(jawaban = "A", narasi = "x", bukti = "")))
  expect_true(h$ok)
  expect_equal(h$data$jawaban_disarankan, "B")
  expect_equal(h$data$unsur_kriteria$status, "tidak_terbukti")
  expect_equal(h$biaya, (1000 * 4 + 200 * 20) / 1e6)
})

test_that("penolakan (refusal) dan error API ditangani tanpa crash", {
  Sys.setenv(ANTHROPIC_API_KEY = "kunci-uji")
  on.exit(Sys.unsetenv("ANTHROPIC_API_KEY"))
  tolak <- function(req) httr2::response_json(body = list(model = "m", stop_reason = "refusal", content = list(),
                                                            usage = list(input_tokens = 5, output_tokens = 0)))
  h <- httr2::with_mocked_responses(tolak, panggil_claude("s", list(list(type = "text", text = "x"))))
  expect_false(h$ok); expect_match(h$pesan, "ditolak")
  galat <- function(req) httr2::response_json(status_code = 401, body = list(error = list(message = "invalid x-api-key")))
  h2 <- httr2::with_mocked_responses(galat, panggil_claude("s", list(list(type = "text", text = "x"))))
  expect_false(h2$ok); expect_match(h2$pesan, "401")
})

test_that("tanpa API key, fitur Claude mengembalikan pesan, bukan error", {
  Sys.unsetenv("ANTHROPIC_API_KEY")
  h <- panggil_claude("s", list())
  expect_false(h$ok)
  expect_match(h$pesan, "ANTHROPIC_API_KEY")
})

test_that("penyimpanan SQLite: simpan, timpa, dan jejak", {
  f <- tempfile(fileext = ".sqlite"); con <- buka_db(f); on.exit(DBI::dbDisconnect(con))
  simpan_isian(con, data.frame(kode = "P1.i.a", jawaban = "Ya", narasi = "n", bukti = "b"), 2026, "uji")
  simpan_isian(con, data.frame(kode = "P1.i.a", jawaban = "Tidak", narasi = "n2", bukti = "b"), 2026, "uji")
  d <- baca_isian(con, 2026)
  expect_equal(nrow(d), 1); expect_equal(d$jawaban, "Tidak")
  expect_equal(nrow(baca_jejak(con, 2026)), 2)
  expect_equal(nrow(baca_isian(con, 2027)), 0)
})

# ---------------------------------------------------------------- modul mutu
test_that("tahap PDCA dikenali dari judul bergaris bawah dan singkatan", {
  pd <- tahap_pdca(c("2026_SK Tim ZI", "Laporan_TL monev triwulan", "Rev_2.i.a dokumen revisi"))
  expect_true(all(pd[c("P", "C", "A")]))
  expect_false(pd[["D"]])
})

test_that("kendali nama dokumen menandai sufiks unduhan berulang", {
  k <- cek_kendali_nama(c("SK Tim (1).pdf", "copy_of notulen", "2026_P1.i.a_SK_Tim"), "P1.i.a")
  expect_length(k$sufiks, 2)
  expect_null(k$tanpa_kode)
})

test_that("pemetaan ISO lengkap dan setiap klausul memiliki nama sesuai standarnya", {
  akar <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  pm <- muat_pemetaan_iso(file.path(akar, "data/pemetaan_iso.csv"))
  expect_setequal(pm$sub_kode, unique(RB$sub_kode[RB$tipe == "butir"]))
  for (s in c("iso9001", "iso37001")) {
    k <- unique(trimws(unlist(strsplit(pm[[s]], ";")))); k <- k[nzchar(k)]
    expect_true(all(k %in% names(KLAUSUL_ISO[[s]])), info = s)
  }
  expect_false(identical(KLAUSUL_ISO$iso9001[["10.2"]], KLAUSUL_ISO$iso37001[["10.2"]]))
})

test_that("skenario konservatif tidak pernah melebihi skor klaim", {
  j <- isi_semua("A")
  isian <- data.frame(kode = names(j), jawaban = unname(j), narasi = "", bukti = "", stringsAsFactors = FALSE)
  st <- status_butir(RB, isian, validasi_semua(RB, isian, NULL, 2026))
  sk <- skenario_skor(RB, isian, st)
  expect_equal(sk$pengungkit[1], 60, tolerance = 1e-9)
  expect_lte(sk$pengungkit[2], sk$pengungkit[1])
})

test_that("interval Wilson berada dalam [0, 1] dan memuat proporsi titik", {
  ci <- ci_wilson(45, 74)
  expect_true(ci[1] >= 0 && ci[2] <= 1 && ci[1] < 45 / 74 && ci[2] > 45 / 74)
  expect_equal(ci_wilson(0, 10)[1], 0)
})

test_that("register tindakan korektif menegakkan syarat verifikasi", {
  con <- buka_db(tempfile(fileext = ".sqlite")); on.exit(DBI::dbDisconnect(con))
  d <- list(kode = "P1.ii.c", temuan = "Sosialisasi tidak berkala", tindakan = "Kalender triwulanan",
            pic = "Tim ZI", tenggat = "2026-12-31", status = "Terverifikasi efektif")
  expect_length(periksa_capa(d), 2)
  d$status <- "Terbuka"; expect_length(periksa_capa(d), 0)
  simpan_capa(con, d, 2026)
  x <- baca_capa(con, 2026); expect_equal(nrow(x), 1)
  simpan_capa(con, modifyList(d, list(status = "Selesai", akar_masalah = "Tidak ada kalender")), 2026, id = x$id)
  expect_equal(baca_capa(con, 2026)$status, "Selesai")
})

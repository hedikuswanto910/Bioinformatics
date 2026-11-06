# =============================================================================
# 70_impor.R - Membaca file Excel LKE (format "Worksheet" Kementerian)
# -----------------------------------------------------------------------------
# Tiga kegunaan:
#   1. bangun_rubrik()      : menyusun daftar pertanyaan, kriteria, dan bobot
#                             (data/rubrik_lke.csv) dari template LKE.
#   2. baca_riwayat()       : mengambil jawaban unit + jawaban & catatan
#                             evaluator (TP PTN, TPSK, TPI) sebagai pelajaran.
#   3. baca_isian_excel()   : mengambil jawaban/narasi/bukti dari file LKE lama
#                             sebagai draf awal isian tahun ini.
#
# Format yang dikenali: sheet "Worksheet" dengan kolom A berisi penanda
# PARENT / PARENT HAVE SUB / SUB / NONE (seperti file Indeks Revisi).
# File tanpa kolom penanda (mis. LKE versi lama) dicocokkan per nomor baris.
# =============================================================================

# Nomor kolom Excel yang dipakai
KOL <- c(A = 1, B = 2, C = 3, D = 4, E = 5, F = 6, G = 7, H = 8, I = 9, J = 10,
         K = 11, L = 12, O = 15, P = 16, Q = 17, U = 21, V = 22, X = 24,
         AC = 29, AD = 30, AF = 32, AK = 37, AL = 38, AN = 40, AP = 42)

baca_sheet_lke <- function(path, sheet = NULL) {
  daftar <- readxl::excel_sheets(path)
  if (is.null(sheet)) {
    kandidat <- daftar[grepl("^Worksheet", daftar)]
    if (length(kandidat) == 0) stop("Sheet 'Worksheet' tidak ditemukan di ", basename(path))
    sheet <- kandidat[1]
  }
  m <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                                           col_types = "text", range = readxl::cell_limits(c(1, 1), c(400, 42))))
  m <- as.data.frame(m, stringsAsFactors = FALSE)
  # pastikan 42 kolom (A..AP) meski kolom kanan kosong
  if (ncol(m) < 42) for (i in (ncol(m) + 1):42) m[[i]] <- NA_character_
  names(m) <- paste0("c", seq_len(ncol(m)))
  m
}

.bersih <- function(x) {
  x <- trimws(gsub("[\r\t]+", " ", x %||% ""))
  x[is.na(x)] <- ""
  x
}
.angka <- function(x) suppressWarnings(as.numeric(x))
.romawi <- function(x) grepl("^(i|ii|iii|iv|v|vi|vii|viii|ix|x)\\.?$", x)

# -----------------------------------------------------------------------------
# 1. Rubrik
# -----------------------------------------------------------------------------
bangun_rubrik <- function(path) {
  m <- baca_sheet_lke(path)
  g <- function(r, kol) .bersih(m[r, KOL[[kol]]])
  if (!any(g(seq_len(nrow(m)), "A") %in% c("PARENT", "PARENT HAVE SUB"))) {
    stop("Kolom A tidak berisi penanda PARENT/SUB. Gunakan template versi Indeks Revisi.")
  }

  hasil <- list()
  bagian <- area <- area_kode <- komp_nama <- sub_nama <- kel_nama <- NA_character_
  komp_no <- 0L; bobot_area <- bobot_komp <- bobot_sub <- bobot_kel <- NA_real_
  sub_kode <- kel_no <- NA; urut_butir <- 0L; induk <- NA_character_; urut_rinci <- 0L
  area_no_hasil <- 0L

  for (r in seq_len(nrow(m))) {
    A <- g(r, "A"); B <- g(r, "B"); C <- g(r, "C"); D <- g(r, "D"); E <- g(r, "E")
    F <- g(r, "F"); G <- g(r, "G"); H <- .angka(g(r, "H")); J <- g(r, "J")

    if (B == "A." && toupper(C) == "PENGUNGKIT") { bagian <- "Pengungkit"; next }
    if (B == "B." && toupper(C) == "HASIL")      { bagian <- "Hasil"; next }
    if (grepl("^TOTAL", toupper(B))) next
    if (is.na(bagian)) next

    # Tingkat area: "I. PEMENUHAN", "II. REFORM", atau area Hasil
    if (C %in% c("I.", "II.", "III.") && nzchar(D)) {
      area <- tools::toTitleCase(tolower(D)); bobot_area <- H
      if (bagian == "Pengungkit") {
        area_kode <- if (grepl("PEMENUHAN", toupper(D))) "P" else "R"
      } else {
        area_no_hasil <- area_no_hasil + 1L; area_kode <- paste0("H", area_no_hasil)
      }
      next
    }

    if (bagian == "Pengungkit") {
      # Komponen: "1  MANAJEMEN PERUBAHAN"
      if (!is.na(.angka(D)) && nzchar(E) && !.romawi(E)) {
        komp_no <- as.integer(.angka(D)); komp_nama <- tools::toTitleCase(tolower(E)); bobot_komp <- H
        next
      }
      # Sub-komponen: "i. Penyusunan Tim Kerja"
      if (.romawi(E) && nzchar(F)) {
        sub_kode <- paste0(area_kode, komp_no, ".", sub("\\.$", "", E))
        sub_nama <- F; bobot_sub <- H; kel_nama <- NA; kel_no <- 0L; bobot_kel <- NA
        urut_butir <- 0L
        next
      }
      if (A == "NONE") {   # kelompok di dalam sub-komponen (mis. LHKPN / LHKASN)
        kel_no <- kel_no + 1L; kel_nama <- G; bobot_kel <- H; urut_butir <- 0L
        next
      }
    }

    if (A %in% c("PARENT", "PARENT HAVE SUB") ||
        (bagian == "Hasil" && nzchar(J) && nzchar(E))) {
      urut_butir <- urut_butir + 1L
      if (bagian == "Hasil") {
        huruf <- sub("\\.$", "", D)
        kode <- paste0(area_kode, ".", huruf)
        teks <- E; s_kode <- area_kode; s_nama <- area
      } else {
        huruf <- sub("\\.$", "", trimws(F))
        if (!grepl("^[a-z]$", huruf)) huruf <- as.character(urut_butir)
        prefix <- if (!is.na(kel_nama) && kel_no > 0) paste0(sub_kode, ".", kel_no) else sub_kode
        kode <- paste0(prefix, ".", huruf)
        teks <- sub("^-\\s*", "", G); s_kode <- sub_kode; s_nama <- sub_nama
      }
      pil <- sub("Nilai\\s*\\(0-4\\)", "Nilai (0-4)", gsub("\\s+", " ", J))
      rumus <- NA_character_
      if (A == "PARENT HAVE SUB") {
        rumus <- if (grepl("penurunan pelanggaran", tolower(teks))) "penurunan" else "rasio"
      }
      hasil[[length(hasil) + 1]] <- data.frame(
        kode = kode, baris_excel = r, bagian = bagian, area = area, area_kode = area_kode,
        komponen_no = if (bagian == "Hasil") NA_integer_ else komp_no,
        komponen = if (bagian == "Hasil") area else komp_nama,
        bobot_komponen = if (bagian == "Hasil") bobot_area else bobot_komp,
        sub_kode = s_kode, sub_nama = s_nama,
        bobot_sub = if (bagian == "Hasil") NA_real_ else bobot_sub,
        kelompok = if (bagian == "Hasil") NA_character_ else kel_nama,
        bobot_kelompok = if (bagian == "Hasil") NA_real_ else bobot_kel,
        tipe = "butir", induk = NA_character_, pertanyaan = teks,
        penjelasan = g(r, "I"), pilihan = pil, rumus_persen = rumus,
        bobot_butir = if (bagian == "Hasil") H else NA_real_,
        stringsAsFactors = FALSE)
      induk <- kode; urut_rinci <- 0L
      next
    }

    if (A == "SUB" && !is.na(induk)) {
      urut_rinci <- urut_rinci + 1L
      p <- hasil[[length(hasil)]]
      # baris induk terakhir mungkin sudah rincian; ambil atribut induknya saja
      p$kode <- paste0(induk, ".s", urut_rinci); p$baris_excel <- r
      p$tipe <- "rincian"; p$induk <- induk
      p$pertanyaan <- sub("^-\\s*", "", G); p$penjelasan <- ""
      p$pilihan <- gsub("\\s+", " ", J); p$rumus_persen <- NA_character_
      hasil[[length(hasil) + 1]] <- p
    }
  }
  rb <- do.call(rbind, hasil)
  if (anyDuplicated(rb$kode)) {
    d <- rb$kode[duplicated(rb$kode)]
    rb$kode <- make.unique(rb$kode, sep = "_")
    warning("Kode ganda diberi akhiran: ", paste(unique(d), collapse = ", "))
  }
  rownames(rb) <- NULL
  rb
}

# -----------------------------------------------------------------------------
# 2. Riwayat penilaian (jawaban unit vs evaluator + catatan)
# -----------------------------------------------------------------------------
baca_riwayat <- function(path, rubrik, sumber = basename(path)) {
  m <- baca_sheet_lke(path)
  ambil <- function(kol) .bersih(m[rubrik$baris_excel, KOL[[kol]]])
  teks_file <- tolower(gsub("\\W", "", ifelse(rubrik$bagian == "Hasil",
                                                .bersih(m[rubrik$baris_excel, KOL[["E"]]]),
                                                .bersih(m[rubrik$baris_excel, KOL[["G"]]]))))
  teks_rb <- tolower(gsub("\\W", "", rubrik$pertanyaan))
  cocok <- substr(teks_file, 1, 25) == substr(teks_rb, 1, 25)
  if (mean(cocok) < 0.9) {
    warning(sprintf("%s: hanya %.0f%% teks pertanyaan cocok dengan rubrik (per nomor baris). Periksa versi template.",
                    sumber, 100 * mean(cocok)))
  }
  data.frame(
    sumber = sumber, kode = rubrik$kode, tipe = rubrik$tipe, cocok_teks = cocok,
    jawaban_unit = ambil("K"), jawaban_tp = ambil("U"), jawaban_tpsk = ambil("AC"), jawaban_tpi = ambil("AK"),
    catatan_tp = ambil("X"), catatan_tpsk = ambil("AF"), catatan_tpi = ambil("AN"),
    narasi = ambil("O"), daftar_bukti = ambil("P"), tautan_bukti = ambil("Q"), revisi = ambil("AP"),
    stringsAsFactors = FALSE)
}

# -----------------------------------------------------------------------------
# 3. Isian awal dari file LKE (jawaban, narasi, bukti) -> format penyimpanan
# -----------------------------------------------------------------------------
# Kolom P (judul dokumen) dan Q (tautan) di Excel berisi daftar bernomor.
# Fungsi ini memasangkan keduanya menjadi baris "Judul | tautan".
pasangkan_bukti <- function(judul, tautan) {
  pecah <- function(x) {
    x <- gsub("\r", "", x %||% "")
    bagian <- unlist(strsplit(x, "\n\\s*\n|\n(?=\\s*\\d+\\.\\s)", perl = TRUE))
    bagian <- trimws(gsub("^\\s*\\d+\\.\\s*", "", gsub("\n", " ", bagian)))
    bagian[nzchar(bagian)]
  }
  j <- pecah(judul); t <- pecah(tautan)
  n <- max(length(j), length(t))
  if (n == 0) return("")
  j <- c(j, rep("", n - length(j))); t <- c(t, rep("", n - length(t)))
  paste(trimws(paste(j, t, sep = " | ")), collapse = "\n")
}

baca_isian_excel <- function(path, rubrik, tahun = KONFIG$tahun) {
  rw <- baca_riwayat(path, rubrik)
  data.frame(
    kode = rw$kode, tahun = as.integer(tahun),
    jawaban = ifelse(rubrik$rumus_persen %in% c("rasio", "penurunan"), "", rw$jawaban_unit),
    narasi = rw$narasi,
    bukti = mapply(pasangkan_bukti, rw$daftar_bukti, rw$tautan_bukti, USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
}

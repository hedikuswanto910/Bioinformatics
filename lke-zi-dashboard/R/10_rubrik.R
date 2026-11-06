# =============================================================================
# 10_rubrik.R - Rubrik LKE dan perhitungan nilai
# -----------------------------------------------------------------------------
# Rumus mengikuti persis rumus di file Excel LKE Kementerian:
#   * Ya/Tidak        : Ya = 1, Tidak = 0
#   * A/B/C           : 1 / 0.5 / 0
#   * A/B/C/D         : 1 / 0.67 / 0.33 / 0
#   * A/B/C/D/E       : 1 / 0.75 / 0.5 / 0.25 / 0
#   * %               : dihitung dari rincian (Jumlah), maksimal 100%
#   * Nilai (0-4)     : nilai / 4
#   * Sub-komponen    : rata-rata nilai butir x bobot sub-komponen
#   * Komponen / Area : penjumlahan
#
# PERBEDAAN SENGAJA dengan Excel: Excel memakai AVERAGE() yang MENGABAIKAN
# butir "Blm Diisi", sehingga butir kosong justru tidak menurunkan nilai.
# Dashboard ini secara bawaan menghitung butir kosong = 0 (konservatif).
# Pakai kosong_nol = FALSE untuk meniru perilaku Excel.
# =============================================================================

muat_rubrik <- function(path = KONFIG$file_rubrik) {
  rb <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8", na.strings = c("", "NA"))
  rb$penjelasan[is.na(rb$penjelasan)] <- ""
  rb
}

PETA_NILAI <- list(
  "Ya/Tidak"  = c(Ya = 1, Tidak = 0),
  "A/B/C"     = c(A = 1, B = 0.5, C = 0),
  "A/B/C/D"   = c(A = 1, B = 0.67, C = 0.33, D = 0),
  "A/B/C/D/E" = c(A = 1, B = 0.75, C = 0.5, D = 0.25, E = 0)
)

pilihan_jawaban <- function(pilihan) {
  if (!is.null(PETA_NILAI[[pilihan]])) names(PETA_NILAI[[pilihan]]) else character(0)
}

# Ubah teks angka "0,5" / "50%" / "0.5" menjadi angka
ke_angka <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x) || is.na(x)) return(NA_real_)
  persen <- grepl("%$", x)
  v <- suppressWarnings(as.numeric(sub(",", ".", sub("%$", "", x), fixed = TRUE)))
  if (persen) v <- v / 100
  v
}

# Nilai 0..1 untuk satu butir kategorik
nilai_pilihan <- function(pilihan, jawaban) {
  peta <- PETA_NILAI[[pilihan]]
  if (is.null(peta)) return(NA_real_)
  j <- trimws(as.character(jawaban %||% ""))
  if (j %in% names(peta)) unname(peta[[j]]) else NA_real_
}

# Persentase dari rincian. Mengembalikan list(nilai, pesan)
#  rasio     : rincian terakhir / rincian pertama  (dibatasi maksimal 1)
#  penurunan : (tahun lalu - tahun ini) / tahun lalu; bila keduanya 0 -> 1
hitung_persen <- function(rumus, v) {
  if (length(v) < 2 || any(is.na(v))) return(list(nilai = NA_real_, pesan = "Rincian jumlah belum lengkap"))
  if (rumus == "penurunan") {
    lalu <- v[1]; kini <- v[2]
    if (lalu == 0 && kini == 0) return(list(nilai = 1, pesan = ""))
    if (lalu == 0) return(list(nilai = 0, pesan = "Tahun lalu 0 pelanggaran, tahun ini ada pelanggaran"))
    return(list(nilai = max(0, min(1, (lalu - kini) / lalu)), pesan = ""))
  }
  penyebut <- v[1]; pembilang <- v[length(v)]
  if (penyebut == 0) return(list(nilai = NA_real_, pesan = "Penyebut bernilai 0 (Excel akan menampilkan #DIV/0!)"))
  list(nilai = min(1, pembilang / penyebut), pesan = "")
}

# -----------------------------------------------------------------------------
# Nilai setiap butir. `jawaban` = vektor bernama (nama = kode), termasuk rincian.
# -----------------------------------------------------------------------------
nilai_butir <- function(rubrik, jawaban) {
  ambil <- function(k) if (k %in% names(jawaban)) jawaban[[k]] else ""
  bt <- rubrik[rubrik$tipe == "butir", ]
  nilai <- numeric(nrow(bt)); pesan <- character(nrow(bt))
  for (i in seq_len(nrow(bt))) {
    b <- bt[i, ]
    if (b$pilihan %in% names(PETA_NILAI)) {
      nilai[i] <- nilai_pilihan(b$pilihan, ambil(b$kode))
    } else if (b$pilihan == "%" && !is.na(b$rumus_persen)) {
      rinci <- rubrik$kode[rubrik$tipe == "rincian" & rubrik$induk %in% b$kode]
      h <- hitung_persen(b$rumus_persen, vapply(rinci, function(k) ke_angka(ambil(k)), numeric(1)))
      nilai[i] <- h$nilai; pesan[i] <- h$pesan
    } else if (b$pilihan == "%") {
      nilai[i] <- min(1, ke_angka(ambil(b$kode)))
    } else if (b$pilihan == "Nilai (0-4)") {
      nilai[i] <- ke_angka(ambil(b$kode)) / 4
    } else {
      nilai[i] <- NA_real_
    }
  }
  data.frame(kode = bt$kode, nilai = nilai, pesan_hitung = pesan, stringsAsFactors = FALSE)
}

.rata <- function(x, kosong_nol) {
  if (kosong_nol) { x[is.na(x)] <- 0; mean(x) } else if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# -----------------------------------------------------------------------------
# Rekap nilai: sub-komponen, komponen, area, dan total
# -----------------------------------------------------------------------------
hitung_skor <- function(rubrik, jawaban, kosong_nol = TRUE) {
  nb <- nilai_butir(rubrik, jawaban)
  bt <- merge(rubrik[rubrik$tipe == "butir", ], nb, by = "kode", sort = FALSE)
  bt <- bt[order(bt$baris_excel), ]

  # --- Pengungkit: per sub-komponen
  pg <- bt[bt$bagian == "Pengungkit", ]
  subs <- unique(pg[, c("sub_kode", "sub_nama", "area", "area_kode", "komponen_no", "komponen", "bobot_komponen", "bobot_sub")])
  subs$skor <- vapply(subs$sub_kode, function(s) {
    x <- pg[pg$sub_kode == s, ]
    if (any(!is.na(x$kelompok))) {
      kel <- unique(x$kelompok[!is.na(x$kelompok)])
      v <- vapply(kel, function(k) .rata(x$nilai[x$kelompok %in% k], kosong_nol) * x$bobot_kelompok[x$kelompok %in% k][1], numeric(1))
      .rata(v, kosong_nol) * x$bobot_sub[1]
    } else {
      .rata(x$nilai, kosong_nol) * x$bobot_sub[1]
    }
  }, numeric(1))

  komp <- stats::aggregate(skor ~ area + area_kode + komponen_no + komponen + bobot_komponen,
                           data = transform(subs, skor = ifelse(is.na(skor), 0, skor)), FUN = sum)
  komp <- komp[order(komp$area_kode, komp$komponen_no), ]

  # --- Hasil: nilai x bobot butir
  hs <- bt[bt$bagian == "Hasil", ]
  hs$skor <- ifelse(is.na(hs$nilai), if (kosong_nol) 0 else NA, hs$nilai) * hs$bobot_butir

  pemenuhan <- sum(komp$skor[komp$area_kode == "P"])
  reform    <- sum(komp$skor[komp$area_kode == "R"])
  hasil     <- sum(hs$skor, na.rm = TRUE)
  list(
    butir = bt, sub = subs, komponen = komp, hasil = hs,
    ringkas = data.frame(
      bagian = c("Pemenuhan", "Reform", "Pengungkit", "Hasil", "Total"),
      bobot  = c(30, 30, 60, 40, 100),
      skor   = c(pemenuhan, reform, pemenuhan + reform, hasil, pemenuhan + reform + hasil))
  )
}

# Ambil isian dalam bentuk vektor bernama untuk hitung_skor()
jawaban_dari_isian <- function(isian) {
  if (is.null(isian) || nrow(isian) == 0) return(character(0))
  stats::setNames(as.character(isian$jawaban), isian$kode)
}

# Pecah teks penjelasan "a. Jika ... b. Jika ..." menjadi kriteria per huruf
kriteria_per_pilihan <- function(penjelasan, pilihan) {
  hur <- pilihan_jawaban(pilihan)
  if (pilihan == "Ya/Tidak") return(c(Ya = penjelasan, Tidak = "Jika kriteria 'Ya' tidak terpenuhi."))
  if (length(hur) == 0) return(character(0))
  teks <- gsub("\r", "", penjelasan)
  posisi <- gregexpr("(^|\\n|\\s)([a-e])\\.\\s", teks, perl = TRUE)[[1]]
  if (posisi[1] == -1) return(stats::setNames(rep(penjelasan, length(hur)), hur))
  awal <- as.integer(posisi) + attr(posisi, "match.length")
  akhir <- c(as.integer(posisi)[-1] - 1, nchar(teks))
  potong <- trimws(substring(teks, awal, akhir))
  potong <- potong[seq_len(min(length(potong), length(hur)))]
  hasil <- stats::setNames(rep("", length(hur)), hur)
  hasil[seq_along(potong)] <- potong
  hasil
}

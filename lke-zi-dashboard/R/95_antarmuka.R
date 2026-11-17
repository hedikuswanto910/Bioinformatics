# =============================================================================
# 95_antarmuka.R - Animasi (anime.js, Motion) dan grafik bergaya shadcn/ui
#                  charts (Recharts) untuk Shiny
# -----------------------------------------------------------------------------
# Seluruh kode JavaScript dibundel di www/vendor/zi-ui.min.js (sumber di
# folder js/). R hanya menyiapkan spesifikasi grafik; menjalankan aplikasi
# TIDAK memerlukan Node.js.
#
# Spesifikasi grafik (list) yang dikirim ke peramban:
#   judul      : teks untuk pembaca layar
#   data       : data frame; kolom `label` (dan opsional `pendek` untuk layar
#                sempit) serta satu kolom per seri
#   seri       : list(list(key, label, warna), ...)
#   tumpuk     : TRUE untuk batang bertumpuk
#   satuan     : mis. "%" ; desimal : jumlah angka desimal di tooltip
#   domain     : rentang sumbu nilai, mis. c(0, 100)
#   label_nilai: TRUE untuk menampilkan angka di ujung batang
# =============================================================================

# Palet grafik (variabel CSS dipakai oleh komponen grafik di peramban)
WARNA_GRAFIK <- list(
  klaim = "var(--chart-1)", tpi = "var(--chart-2)", unit_lalu = "var(--chart-3)",
  `belum diisi` = "var(--status-belum)", merah = "var(--status-merah)",
  kuning = "var(--status-kuning)", hijau = "var(--status-hijau)")

zi_chart_output <- function(id, tinggi_min = "14rem") {
  htmltools::div(id = id, class = "zi-chart", style = sprintf("min-height:%s;", tinggi_min))
}

render_zi_chart <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  shiny::createRenderFunction(func, function(value, session, name, ...) value, outputFunc = zi_chart_output)
}

.label_komponen <- function(no, area_kode, nama) paste0("Zona ", no, " (", area_kode, ") ", nama)
.label_pendek   <- function(no, area_kode) paste0("Zona ", no, " (", area_kode, ")")

# Perbandingan skor klaim dengan penilaian terdahulu per komponen (% bobot)
spek_grafik_komponen <- function(rubrik, skor, riwayat = NULL) {
  kp <- skor$komponen
  d <- data.frame(label = .label_komponen(kp$komponen_no, kp$area_kode, kp$komponen),
                  pendek = .label_pendek(kp$komponen_no, kp$area_kode),
                  klaim = round(100 * kp$skor / kp$bobot_komponen, 1), stringsAsFactors = FALSE)
  seri <- list()
  sumber_lalu <- c(jawaban_unit = "unit_lalu", jawaban_tpi = "tpi")
  label_lalu  <- c(unit_lalu = "Klaim unit (lalu)", tpi = "TPI (lalu)")
  if (!is.null(riwayat)) {
    for (kol in names(sumber_lalu)) {
      if (!kol %in% names(riwayat) || !any(nzchar(riwayat[[kol]]))) next
      k2 <- hitung_skor(rubrik, stats::setNames(riwayat[[kol]], riwayat$kode), kosong_nol = FALSE)$komponen
      kunci <- sumber_lalu[[kol]]
      d[[kunci]] <- round(100 * k2$skor / k2$bobot_komponen, 1)[match(d$label, .label_komponen(k2$komponen_no, k2$area_kode, k2$komponen))]
      seri[[length(seri) + 1]] <- list(key = kunci, label = label_lalu[[kunci]], warna = WARNA_GRAFIK[[kunci]])
    }
  }
  seri[[length(seri) + 1]] <- list(key = "klaim", label = "Klaim saat ini", warna = WARNA_GRAFIK$klaim)
  d <- d[order(d$label), ]
  list(judul = "Perbandingan skor klaim dan penilaian evaluator terdahulu per komponen",
       data = d, seri = seri, satuan = "%", desimal = 1, domain = c(0, 100), label_nilai = FALSE)
}

# Distribusi status validasi butir per komponen (batang bertumpuk)
spek_grafik_status <- function(status, butir) {
  st <- merge(status, unique(butir[, c("kode", "zona", "nama_zona")]), by = "kode")
  st$label  <- ifelse(st$zona == "H", "Hasil", paste0(st$nama_zona, " (", substr(st$area, 1, 1), ") ", st$komponen))
  st$pendek <- ifelse(st$zona == "H", "Hasil", paste0(st$nama_zona, " (", substr(st$area, 1, 1), ")"))
  tingkat <- c("hijau", "kuning", "merah", "belum diisi")
  tb <- as.data.frame.matrix(table(st$label, factor(st$status, tingkat)))
  d <- data.frame(label = rownames(tb), pendek = st$pendek[match(rownames(tb), st$label)], stringsAsFactors = FALSE)
  kunci <- c(hijau = "hijau", kuning = "kuning", merah = "merah", `belum diisi` = "belum")
  for (t in tingkat) d[[kunci[[t]]]] <- tb[[t]]
  nama <- c(hijau = "Hijau", kuning = "Kuning", merah = "Merah", `belum diisi` = "Belum diisi")
  list(judul = "Jumlah butir per status validasi pada setiap komponen", data = d, tumpuk = TRUE,
       seri = lapply(tingkat, function(t) list(key = kunci[[t]], label = nama[[t]], warna = WARNA_GRAFIK[[t]])),
       satuan = "", desimal = 0, label_nilai = FALSE, tinggi_min = 200)
}

# CSS komponen grafik (meniru tampilan shadcn/ui charts dengan palet marun-emas)
CSS_GRAFIK <- isi_warna("
:root { --chart-1: {{marun}}; --chart-2: {{emas}}; --chart-3: #E8B4AE;
        --status-hijau: #1E7B4A; --status-kuning: #D99A00; --status-merah: #B42318; --status-belum: #A8A29E;
        --zi-teks: #3A1010; --zi-teks-redup: #7A6A6A; --zi-grid: #EADADA; --zi-kursor: rgba(122,31,31,.06); }
.zi-chart { width: 100%; position: relative; }
.zi-grafik { width: 100%; }
.zi-grafik .recharts-surface { overflow: visible; }
.zi-grafik .recharts-cartesian-axis-tick-value { font-family: inherit; }
.zi-legenda { display: flex; flex-wrap: wrap; gap: .35rem .9rem; justify-content: center; margin: 0 0 .5rem; font-size: .8rem; color: var(--zi-teks); }
.zi-legenda-item { display: inline-flex; align-items: center; gap: .4rem; }
.zi-legenda-titik { width: .65rem; height: .65rem; border-radius: 3px; display: inline-block; }
.zi-tooltip { min-width: 10rem; background: #fff; border: 1px solid #EADADA; border-radius: .6rem; padding: .5rem .65rem;
              box-shadow: 0 10px 24px -8px rgba(78,7,0,.25); font-size: .78rem; color: var(--zi-teks); }
.zi-tooltip-judul { font-weight: 600; margin-bottom: .3rem; }
.zi-tooltip-baris { display: grid; grid-template-columns: .65rem 1fr auto; align-items: center; gap: .45rem; line-height: 1.6; }
.zi-tooltip-indikator { width: .65rem; height: .65rem; border-radius: 2px; }
.zi-tooltip-label { color: var(--zi-teks-redup); }
.zi-tooltip-nilai { font-weight: 600; font-variant-numeric: tabular-nums; text-align: right; }
.zi-grafik-catatan, .zi-grafik-galat { font-size: .8rem; color: var(--zi-teks-redup); margin-top: .4rem; }
@media (prefers-reduced-motion: reduce) { .kpi-kartu, .card { transition: none !important; } }
")

zi_ui_head <- function() {
  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(CSS_GRAFIK)),
    htmltools::tags$script(src = paste0("vendor/zi-ui.min.js?v=", as.integer(file.mtime("www/vendor/zi-ui.min.js"))), defer = NA))
}

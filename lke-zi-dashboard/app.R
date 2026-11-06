# =============================================================================
# Dashboard LKE Zona Integritas - aplikasi Shiny
# Jalankan dari RStudio: buka file ini lalu klik "Run App",
# atau dari konsol R:  shiny::runApp("lke-zi-dashboard")
# =============================================================================

library(shiny)
library(bslib)

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f, encoding = "UTF-8")

options(shiny.maxRequestSize = 60 * 1024^2)  # unggahan maks 60 MB

RB <- muat_rubrik()
RB$grup <- ifelse(RB$bagian == "Hasil", RB$area_kode, paste0(RB$area_kode, RB$komponen_no))
BUTIR <- RB[RB$tipe == "butir", ]
BUTIR <- BUTIR[order(BUTIR$baris_excel), ]

LAMBANG <- c(`belum diisi` = "\u26AA", merah = "\U0001F534", kuning = "\U0001F7E1", hijau = "\U0001F7E2")
WARNA_STATUS <- c(`belum diisi` = "#6c757d", merah = "#dc3545", kuning = "#e0a800", hijau = "#198754")

pilihan_grup <- function(area) {
  x <- unique(BUTIR[BUTIR$area_kode %in% area | (area == "H" & BUTIR$bagian == "Hasil"), c("grup", "komponen")])
  stats::setNames(x$grup, paste(sub("^[PRH]", "", x$grup), x$komponen))
}

kotak_temuan <- function(tm) {
  if (is.null(tm) || nrow(tm) == 0) return(div(class = "alert alert-success mb-2", "Tidak ada temuan validasi otomatis untuk isian ini."))
  urut <- tm[order(match(tm$tingkat, c("merah", "kuning", "info"))), ]
  kelas <- c(merah = "alert-danger", kuning = "alert-warning", info = "alert-secondary")
  tagList(lapply(seq_len(nrow(urut)), function(i) {
    div(class = paste("alert py-2 mb-2", kelas[[urut$tingkat[i]]]),
        tags$b(paste0("[", urut$aturan[i], "] ")), urut$pesan[i])
  }))
}

teks_pra <- function(...) tags$div(style = "white-space: pre-wrap; font-size: .9rem;", ...)

# =============================================================================
# UI
# =============================================================================
ui <- page_navbar(
  id = "nav",
  title = "LKE Zona Integritas",
  theme = bs_theme(version = 5, primary = "#1F3864"),
  header = tags$head(tags$style(HTML("
    .daftar-butir .radio label { font-size: .85rem; line-height: 1.25; }
    .daftar-butir .shiny-options-group { max-height: 62vh; overflow-y: auto; }
    .kriteria td { vertical-align: top; padding: 4px 8px; }
  "))),

  # ---------------------------------------------------------------- Beranda
  nav_panel("Beranda",
    layout_columns(fill = FALSE,
      value_box("Nilai klaim Pengungkit", textOutput("vb_pengungkit"), showcase = icon("gauge-high"), theme = "primary"),
      value_box("Nilai klaim Hasil", textOutput("vb_hasil"), showcase = icon("chart-line"), theme = "secondary"),
      value_box("Butir siap (hijau)", textOutput("vb_hijau"), showcase = icon("circle-check"), theme = "success"),
      value_box("Butir perlu perbaikan", textOutput("vb_merah"), showcase = icon("triangle-exclamation"), theme = "danger")
    ),
    layout_columns(col_widths = c(7, 5),
      card(card_header("Klaim saat ini vs penilaian evaluator sebelumnya (% dari bobot)"),
           plotOutput("plot_komponen", height = "420px")),
      card(card_header("Progres per komponen"), DT::DTOutput("tbl_progres"))
    ),
    card(card_header("Cara memakai"), markdown("
1. **Isi LKE**: kerjakan butir berurutan. Setiap butir menampilkan kriteria, catatan evaluator periode lalu, dan validasi otomatis yang langsung berubah saat Anda mengetik.
2. **Validasi**: lihat semua butir merah/kuning, klik baris untuk langsung menuju butir itu.
3. **Claude** (opsional): pra-penilaian butir berdasarkan narasi + PDF bukti, dan draf narasi tanpa mengarang fakta.
4. **Ekspor**: unduh lembar isian Excel (urutan sama dengan LKE) dan laporan validasi Word untuk rapat Tim ZI.

Nilai di halaman ini adalah **nilai yang diklaim unit**, bukan prediksi nilai evaluator. Butir kosong dihitung 0.
"))
  ),

  # ---------------------------------------------------------------- Isi LKE
  nav_panel("Isi LKE",
    layout_sidebar(
      sidebar = sidebar(width = 380,
        selectInput("f_area", "Area", c("Pemenuhan" = "P", "Reform" = "R", "Hasil" = "H")),
        selectInput("f_grup", "Komponen", pilihan_grup("P")),
        selectInput("f_status", "Tampilkan", c("Semua", "belum diisi", "merah", "kuning", "hijau")),
        div(class = "daftar-butir", uiOutput("ui_daftar_butir"))
      ),
      uiOutput("ui_form")
    )
  ),

  # ---------------------------------------------------------------- Validasi
  nav_panel("Validasi",
    card(card_header("Status seluruh butir - klik baris untuk membuka butir"),
         layout_columns(fill = FALSE, col_widths = c(3, 3),
           selectInput("v_status", "Status", c("Semua", "merah", "kuning", "belum diisi", "hijau")),
           selectInput("v_area", "Area", c("Semua", "Pemenuhan", "Reform", "Hasil"))),
         DT::DTOutput("tbl_validasi")),
    card(card_header("Seluruh temuan"), DT::DTOutput("tbl_temuan"))
  ),

  # ---------------------------------------------------------------- Analisis
  nav_panel("Pelajaran Penilaian Lalu",
    uiOutput("ui_analisis")
  ),

  # ---------------------------------------------------------------- Dokumen
  nav_panel("Draf Dokumen (Claude)",
    layout_sidebar(
      sidebar = sidebar(width = 420,
        selectInput("d_jenis", "Jenis dokumen", JENIS_DOKUMEN),
        selectizeInput("d_butir", "Butir LKE terkait (opsional)", c("(tidak ada)" = "", stats::setNames(BUTIR$kode, paste(BUTIR$kode, substr(BUTIR$pertanyaan, 1, 60))))),
        textAreaInput("d_fakta", "Fakta kegiatan yang SUDAH terjadi", rows = 14,
          placeholder = "Contoh:\n- Rapat monev ZI triwulan II, 14 Juli 2026, R. Senat FK\n- Dipimpin Wakil Dekan II; 23 peserta (daftar hadir terlampir)\n- Temuan: 3 SOP belum dievaluasi (Legalisir, Cuti, Peminjaman Ruang)\n- Keputusan: evaluasi selesai 30 Agustus, PIC Kasubag Akademik"),
        actionButton("d_buat", "Buat draf", class = "btn-primary"),
        helpText("Claude dilarang mengarang fakta; bagian yang tidak ada datanya ditandai [PERLU DIISI]. Dokumen bukti yang tidak sesuai kenyataan adalah pemalsuan.")
      ),
      card(card_header("Draf"), uiOutput("d_hasil"), downloadButton("d_unduh", "Unduh .docx"))
    )
  ),

  # ---------------------------------------------------------------- SOP
  nav_panel("Prosedur Mutu (SOP)",
    layout_sidebar(
      sidebar = sidebar(width = 360,
        fileInput("s_daftar", "Daftar PM (.xlsx)", accept = ".xlsx"),
        fileInput("s_pdf", "File PDF SOP (boleh banyak)", accept = ".pdf", multiple = TRUE),
        actionButton("s_periksa", "Periksa", class = "btn-primary"),
        helpText("Memeriksa nomor dokumen ganda, nama file vs nomor dokumen, versi daftar vs nomor revisi di PDF, kop 'Halaman x dari N', dan status upload.")
      ),
      card(card_header("Temuan"), DT::DTOutput("s_temuan")),
      card(card_header("Isi PDF yang terbaca"), DT::DTOutput("s_pdf_tbl"))
    )
  ),

  # ---------------------------------------------------------------- Ekspor
  nav_panel("Ekspor",
    layout_columns(
      card(card_header("Lembar isian Excel"),
           p("Workbook baru berisi seluruh isian, status, nilai, dan temuan; kolom 'Baris LKE' = nomor baris di template resmi."),
           downloadButton("e_excel", "Unduh Excel")),
      card(card_header("Laporan validasi Word"),
           p("Ringkasan nilai klaim, status, dan daftar temuan per butir untuk rapat Tim ZI."),
           downloadButton("e_docx", "Unduh Word"))
    )
  ),

  # ---------------------------------------------------------------- Pengaturan
  nav_panel("Pengaturan",
    layout_columns(col_widths = c(6, 6),
      card(card_header("Umum & Claude"),
           numericInput("p_tahun", "Tahun penilaian", KONFIG$tahun, min = 2020, max = 2100),
           uiOutput("p_status_ai"),
           selectInput("p_effort", "Tingkat penalaran Claude (effort)", c("low", "medium", "high", "xhigh"), selected = KONFIG$effort),
           helpText("Effort lebih tinggi = lebih teliti tetapi lebih lama dan mahal.")),
      card(card_header("Impor dari file LKE Excel"),
           fileInput("p_file_isian", "Isian awal dari file LKE (jawaban, narasi, bukti)", accept = ".xlsx"),
           checkboxInput("p_timpa", "Timpa butir yang sudah diisi", FALSE),
           actionButton("p_impor_isian", "Impor isian"),
           tags$hr(),
           fileInput("p_file_riwayat", "Riwayat penilaian evaluator (file yang memuat kolom TP PTN/TPSK/TPI)", accept = ".xlsx"),
           actionButton("p_impor_riwayat", "Impor riwayat"))
    ),
    card(card_header("Pemakaian Claude"), textOutput("p_biaya"), DT::DTOutput("p_log_ai")),
    card(card_header("Jejak perubahan"), DT::DTOutput("p_jejak"))
  ),
  nav_spacer(),
  nav_item(tags$small(class = "text-muted", KONFIG$unit))
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  con <- buka_db()
  onStop(function() DBI::dbDisconnect(con))

  tahun   <- reactive(as.integer(input$p_tahun %||% KONFIG$tahun))
  pemicu  <- reactiveVal(0)                     # naikkan untuk memuat ulang isian
  riwayat <- reactiveVal(muat_riwayat())
  isian   <- reactive({ pemicu(); baca_isian(con, tahun()) })
  temuan  <- reactive(validasi_semua(RB, isian(), riwayat(), tahun()))
  status  <- reactive(status_butir(RB, isian(), temuan()))
  skor    <- reactive(hitung_skor(RB, jawaban_dari_isian(isian())))
  hasil_ai <- reactiveVal(NULL)
  draf_dok <- reactiveVal(NULL)
  target_butir <- reactiveVal(NULL)               # butir tujuan saat pindah komponen

  ambil_isi <- function(kode, kol) { d <- isian(); v <- d[[kol]][d$kode == kode]; if (length(v)) v[1] %||% "" else "" }
  ambil_rw  <- function(kode) { rw <- riwayat(); if (is.null(rw)) return(NULL); x <- rw[rw$kode == kode, ]; if (nrow(x)) x[1, ] else NULL }

  # ------------------------------------------------------------- Beranda
  output$vb_pengungkit <- renderText(sprintf("%.2f / 60", skor()$ringkas$skor[3]))
  output$vb_hasil      <- renderText(sprintf("%.2f / 40", skor()$ringkas$skor[4]))
  output$vb_hijau      <- renderText(sprintf("%d / %d", sum(status()$status == "hijau"), nrow(status())))
  output$vb_merah      <- renderText(sprintf("%d merah, %d kuning", sum(status()$status == "merah"), sum(status()$status == "kuning")))

  output$plot_komponen <- renderPlot({
    kp <- skor()$komponen
    d <- data.frame(komponen = paste0(kp$area_kode, kp$komponen_no, " ", kp$komponen),
                    sumber = "Klaim saat ini", persen = 100 * kp$skor / kp$bobot_komponen)
    rw <- riwayat()
    sumber_lalu <- c(jawaban_unit = "Klaim unit (lalu)", jawaban_tpi = "TPI (lalu)")
    if (!is.null(rw)) {
      for (kol in names(sumber_lalu)) {
        if (!any(nzchar(rw[[kol]]))) next
        k2 <- hitung_skor(RB, stats::setNames(rw[[kol]], rw$kode), kosong_nol = FALSE)$komponen
        d <- rbind(d, data.frame(komponen = paste0(k2$area_kode, k2$komponen_no, " ", k2$komponen), sumber = sumber_lalu[[kol]],
                                 persen = 100 * k2$skor / k2$bobot_komponen))
      }
    }
    d$sumber <- factor(d$sumber, c(unname(sumber_lalu), "Klaim saat ini"))
    d$komponen <- factor(d$komponen, rev(sort(unique(d$komponen))))
    ggplot2::ggplot(d, ggplot2::aes(x = persen, y = komponen, fill = sumber)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
      ggplot2::scale_fill_manual(values = c("Klaim unit (lalu)" = "#9DB4D6", "TPI (lalu)" = "#C0392B", "Klaim saat ini" = "#1F3864"), drop = TRUE) +
      ggplot2::scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
      ggplot2::labs(x = "% dari bobot komponen", y = NULL, fill = NULL) +
      ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "top", panel.grid.major.y = ggplot2::element_blank())
  })

  output$tbl_progres <- DT::renderDT({
    st <- merge(status(), unique(BUTIR[, c("kode", "grup")]), by = "kode")
    tb <- as.data.frame.matrix(table(paste(st$grup, st$komponen), factor(st$status, names(LAMBANG))))
    tb <- cbind(Komponen = rownames(tb), tb); rownames(tb) <- NULL
    DT::datatable(tb, rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })

  # ------------------------------------------------------------- Isi LKE: navigasi
  observeEvent(input$f_area, {
    pil <- pilihan_grup(input$f_area)
    tgt <- target_butir()
    g <- if (!is.null(tgt)) BUTIR$grup[BUTIR$kode == tgt] else NULL
    updateSelectInput(session, "f_grup", choices = pil, selected = if (length(g) && g %in% pil) g else pil[1])
  }, ignoreInit = TRUE)

  daftar_terlihat <- reactive({
    req(input$f_grup)
    st <- status(); x <- BUTIR[BUTIR$grup == input$f_grup, ]
    x$status <- st$status[match(x$kode, st$kode)]
    if (input$f_status != "Semua") x <- x[x$status == input$f_status, ]
    x
  })

  output$ui_daftar_butir <- renderUI({
    x <- daftar_terlihat()
    if (nrow(x) == 0) return(helpText("Tidak ada butir dengan status ini."))
    pilih <- isolate(target_butir() %||% input$butir_pilih)
    if (!is.null(isolate(target_butir())) && isolate(target_butir()) %in% x$kode) target_butir(NULL)
    radioButtons("butir_pilih", NULL,
                 choiceNames = lapply(seq_len(nrow(x)), function(i) paste(LAMBANG[[x$status[i]]], x$kode[i], "-", substr(x$pertanyaan[i], 1, 80))),
                 choiceValues = x$kode, selected = if (!is.null(pilih) && pilih %in% x$kode) pilih else x$kode[1])
  })

  butir_aktif <- reactive({ req(input$butir_pilih); BUTIR[BUTIR$kode == input$butir_pilih, ] })
  rinci_aktif <- reactive(RB[RB$tipe == "rincian" & RB$induk %in% butir_aktif()$kode, ])

  buka_butir <- function(kode) {
    b <- BUTIR[BUTIR$kode == kode, ]
    area <- if (b$bagian == "Hasil") "H" else b$area_kode
    updateSelectInput(session, "f_area", selected = area)
    updateSelectInput(session, "f_grup", choices = pilihan_grup(area), selected = b$grup)
    updateSelectInput(session, "f_status", selected = "Semua")
    target_butir(kode)
    updateRadioButtons(session, "butir_pilih", selected = kode)
    hasil_ai(NULL)
  }
  observeEvent(input$btn_sebelum, { i <- match(input$butir_pilih, BUTIR$kode); if (i > 1) buka_butir(BUTIR$kode[i - 1]) })
  observeEvent(input$btn_lanjut,  { i <- match(input$butir_pilih, BUTIR$kode); if (i < nrow(BUTIR)) buka_butir(BUTIR$kode[i + 1]) })
  observeEvent(input$butir_pilih, hasil_ai(NULL))

  # ------------------------------------------------------------- Isi LKE: formulir
  output$ui_form <- renderUI({
    b <- butir_aktif(); rinci <- rinci_aktif(); k <- b$kode
    isi <- isolate(list(jawaban = ambil_isi(k, "jawaban"), narasi = ambil_isi(k, "narasi"),
                        bukti = ambil_isi(k, "bukti"), pic = ambil_isi(k, "pic")))
    rw <- isolate(ambil_rw(k))
    krit <- kriteria_per_pilihan(b$penjelasan, b$pilihan)
    i <- match(k, BUTIR$kode)

    input_jawaban <- if (b$pilihan %in% names(PETA_NILAI)) {
      radioButtons("in_jawaban", "Jawaban", choices = pilihan_jawaban(b$pilihan), selected = if (nzchar(isi$jawaban)) isi$jawaban else character(0), inline = TRUE)
    } else if (nrow(rinci) > 0) {
      tagList(tags$label("Rincian (persentase dihitung otomatis)"),
              lapply(seq_len(nrow(rinci)), function(j) numericInput(paste0("in_rinci_", j), rinci$pertanyaan[j],
                                                                    value = ke_angka(isolate(ambil_isi(rinci$kode[j], "jawaban"))), min = 0)),
              uiOutput("ui_persen"))
    } else {
      textInput("in_jawaban", paste("Jawaban", b$pilihan), value = isi$jawaban)
    }

    tagList(
      card(
        card_header(div(class = "d-flex justify-content-between align-items-center",
          span(tags$b(k), " \u00B7 ", b$area, " \u203A ", b$komponen, " \u203A ", b$sub_nama),
          span(actionButton("btn_sebelum", "\u2190 Sebelumnya", class = "btn-sm", disabled = if (i == 1) "disabled" else NULL),
               actionButton("btn_lanjut", "Berikutnya \u2192", class = "btn-sm", disabled = if (i == nrow(BUTIR)) "disabled" else NULL)))),
        h5(b$pertanyaan),
        accordion(open = c("Kriteria jawaban", if (!is.null(rw)) "Catatan evaluator periode lalu"),
          accordion_panel("Kriteria jawaban",
            if (length(krit)) tags$table(class = "kriteria", lapply(names(krit), function(h) tags$tr(tags$td(tags$b(h)), tags$td(krit[[h]]))))
            else teks_pra(b$penjelasan)),
          if (!is.null(rw)) accordion_panel("Catatan evaluator periode lalu",
            tags$p(sprintf("Unit: %s \u00B7 TP PTN: %s \u00B7 TPSK: %s \u00B7 TPI: %s",
                           rw$jawaban_unit %||% "-", rw$jawaban_tp %||% "-", rw$jawaban_tpsk %||% "-", rw$jawaban_tpi %||% "-")),
            if (nzchar(rw$catatan_tpi)) tagList(tags$b("TPI"), teks_pra(rw$catatan_tpi)),
            if (nzchar(rw$catatan_tpsk)) tagList(tags$b("TPSK"), teks_pra(rw$catatan_tpsk)),
            if (nzchar(rw$catatan_tp)) tagList(tags$b("TP PTN"), teks_pra(rw$catatan_tp))),
          if (!is.null(rw) && nzchar(rw$narasi)) accordion_panel("Isian periode lalu",
            tags$b("Narasi"), teks_pra(rw$narasi), tags$b("Bukti"), teks_pra(rw$daftar_bukti),
            actionButton("btn_pakai_lalu", "Pakai isian lalu sebagai draf", class = "btn-sm btn-outline-secondary mt-2"))
        )
      ),
      layout_columns(col_widths = c(7, 5),
        card(card_header("Isian"),
          input_jawaban,
          textAreaInput("in_narasi", "Narasi / Keterangan", value = isi$narasi, rows = 9, width = "100%",
                        placeholder = "Jelaskan: apa yang dilakukan, dasar penetapannya, kapan & seberapa sering, siapa yang terlibat, hasil/dampaknya, monev & tindak lanjut. Rujuk bukti dengan nomor [1], [2]."),
          textAreaInput("in_bukti", "Bukti dukung (satu per baris: Judul dokumen | tautan)", value = isi$bukti, rows = 6, width = "100%",
                        placeholder = "2026_SK Tim Pembangunan ZI FK | https://...\n2026_Notulen Rapat Pembentukan Tim ZI | https://..."),
          textInput("in_pic", "Penanggung jawab (PIC) yang mengisi", value = isi$pic),
          fileInput("in_pdf", "PDF bukti untuk dibaca Claude (opsional)", accept = ".pdf", multiple = TRUE),
          uiOutput("ui_pdf_tersimpan"),
          div(class = "d-flex gap-2 flex-wrap",
            actionButton("btn_simpan", "Simpan", class = "btn-primary"),
            actionButton("btn_ai_validasi", "Pra-penilaian Claude", class = "btn-outline-primary"),
            actionButton("btn_ai_narasi", "Draf narasi Claude", class = "btn-outline-primary"))
        ),
        card(card_header("Validasi otomatis (langsung)"), uiOutput("ui_temuan_live"))
      ),
      uiOutput("ui_hasil_ai")
    )
  })

  # Nilai isian yang sedang diketik (belum disimpan)
  isian_live <- reactive({
    b <- butir_aktif(); rinci <- rinci_aktif()
    v_rinci <- if (nrow(rinci)) stats::setNames(vapply(seq_len(nrow(rinci)), function(j) {
      x <- input[[paste0("in_rinci_", j)]]; if (is.null(x) || is.na(x)) "" else as.character(x) }, character(1)), rinci$kode) else character(0)
    list(jawaban = input$in_jawaban %||% "", narasi = input$in_narasi %||% "", bukti = input$in_bukti %||% "", rinci = v_rinci)
  })

  output$ui_persen <- renderUI({
    b <- butir_aktif(); v <- vapply(isian_live()$rinci, ke_angka, numeric(1))
    h <- hitung_persen(b$rumus_persen %||% "rasio", v)
    tags$p(tags$b("Persentase: "), if (is.na(h$nilai)) h$pesan else sprintf("%.1f%%", 100 * h$nilai))
  })

  temuan_live <- reactive({
    b <- butir_aktif(); il <- isian_live()
    validasi_butir(b, rinci_aktif(), il, il$rinci, ambil_rw(b$kode), tahun())
  })
  output$ui_temuan_live <- renderUI(kotak_temuan(temuan_live()))

  observeEvent(input$btn_pakai_lalu, {
    rw <- ambil_rw(butir_aktif()$kode); req(rw)
    updateTextAreaInput(session, "in_narasi", value = rw$narasi)
    updateTextAreaInput(session, "in_bukti", value = pasangkan_bukti(rw$daftar_bukti, rw$tautan_bukti))
    showNotification("Isian lalu disalin. Perbarui dengan bukti periode ini sebelum disimpan.", type = "warning")
  })

  folder_pdf <- reactive(file.path(KONFIG$folder_unggah, tahun(), butir_aktif()$kode))
  observeEvent(input$in_pdf, {
    dir.create(folder_pdf(), recursive = TRUE, showWarnings = FALSE)
    nama <- gsub("[^A-Za-z0-9._ -]", "_", input$in_pdf$name)
    file.copy(input$in_pdf$datapath, file.path(folder_pdf(), nama), overwrite = TRUE)
    showNotification(sprintf("%d PDF disimpan untuk butir ini.", length(nama)))
  })
  output$ui_pdf_tersimpan <- renderUI({
    input$in_pdf
    f <- list.files(folder_pdf(), "\\.pdf$", ignore.case = TRUE)
    if (length(f)) helpText(paste("PDF tersimpan:", paste(f, collapse = ", ")))
  })

  observeEvent(input$btn_simpan, {
    b <- butir_aktif(); il <- isian_live(); pic <- trimws(input$in_pic %||% "")
    if (!nzchar(pic)) { showNotification("Isi nama PIC terlebih dahulu.", type = "error"); return() }
    df <- data.frame(kode = b$kode, jawaban = il$jawaban, narasi = il$narasi, bukti = il$bukti, stringsAsFactors = FALSE)
    if (length(il$rinci)) df <- rbind(df, data.frame(kode = names(il$rinci), jawaban = unname(il$rinci), narasi = "", bukti = "", stringsAsFactors = FALSE))
    simpan_isian(con, df, tahun(), oleh = pic)
    pemicu(pemicu() + 1)
    tm <- temuan_live(); n_m <- sum(tm$tingkat == "merah"); n_k <- sum(tm$tingkat == "kuning")
    showNotification(sprintf("Tersimpan. %d temuan merah, %d kuning.", n_m, n_k), type = if (n_m) "warning" else "message")
  })

  # ------------------------------------------------------------- Claude
  observeEvent(input$btn_ai_validasi, {
    b <- butir_aktif(); il <- isian_live()
    pdf <- list.files(folder_pdf(), "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
    withProgress(message = "Claude sedang menilai butir ini...", value = 0.3, {
      h <- claude_validasi_butir(b, il, rinci_aktif(), il$rinci, ambil_rw(b$kode), temuan_live(), pdf)
    })
    if (isTRUE(h$ok) || !is.null(h$token_in)) catat_ai(con, b$kode, "validasi", h, tahun())
    hasil_ai(c(h, jenis = "validasi"))
  })
  observeEvent(input$btn_ai_narasi, {
    b <- butir_aktif(); il <- isian_live()
    pdf <- list.files(folder_pdf(), "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
    withProgress(message = "Claude menyusun draf narasi...", value = 0.3, {
      h <- claude_draf_narasi(b, il, rinci_aktif(), il$rinci, ambil_rw(b$kode), pdf)
    })
    if (isTRUE(h$ok) || !is.null(h$token_in)) catat_ai(con, b$kode, "narasi", h, tahun())
    hasil_ai(c(h, jenis = "narasi"))
  })
  observeEvent(input$btn_salin_narasi, {
    h <- hasil_ai(); req(h$data$narasi)
    updateTextAreaInput(session, "in_narasi", value = h$data$narasi)
    showNotification("Draf disalin ke kolom narasi. Lengkapi bagian [PERLU DIISI] lalu simpan.", type = "warning")
  })

  output$ui_hasil_ai <- renderUI({
    h <- hasil_ai(); if (is.null(h)) return(NULL)
    biaya <- if (!is.null(h$token_in)) sprintf("%s \u00B7 %d token masuk, %d token keluar \u00B7 \u2248 US$%.3f", h$model %||% "", h$token_in, h$token_out, h$biaya) else ""
    if (!isTRUE(h$ok)) return(card(card_header("Claude"), div(class = "alert alert-danger", h$pesan), helpText(biaya)))
    d <- h$data
    isi <- if (h$jenis == "validasi") {
      warna <- c(rendah = "success", sedang = "warning", tinggi = "danger")[[d$tingkat_risiko]]
      unsur <- as.data.frame(d$unsur_kriteria)
      tagList(
        p(tags$b("Jawaban yang dapat dipertahankan: "), tags$span(class = "badge bg-primary fs-6", d$jawaban_disarankan),
          " Risiko diturunkan: ", tags$span(class = paste0("badge bg-", warna), d$tingkat_risiko)),
        teks_pra(d$alasan),
        if (nrow(unsur)) tags$table(class = "table table-sm mt-2",
          tags$thead(tags$tr(tags$th("Unsur kriteria"), tags$th("Terbukti?"), tags$th("Keterangan"))),
          tags$tbody(lapply(seq_len(nrow(unsur)), function(i) tags$tr(tags$td(unsur$unsur[i]),
            tags$td(if (isTRUE(unsur$terbukti[i])) "\u2705" else "\u274C"), tags$td(unsur$keterangan[i]))))),
        if (length(d$dokumen_perlu_ditambah)) tagList(tags$b("Dokumen yang perlu ditambahkan"), tags$ul(lapply(d$dokumen_perlu_ditambah, tags$li))),
        tags$b("Perbaikan narasi"), teks_pra(d$perbaikan_narasi),
        if (length(h$pdf_dilewati)) div(class = "alert alert-warning mt-2", "PDF tidak dikirim karena melebihi batas ukuran: ", paste(basename(h$pdf_dilewati), collapse = ", "))
      )
    } else {
      tagList(teks_pra(d$narasi),
              if (length(d$data_perlu_dilengkapi)) tagList(tags$b("Data yang perlu dilengkapi"), tags$ul(lapply(d$data_perlu_dilengkapi, tags$li))),
              actionButton("btn_salin_narasi", "Salin ke kolom narasi", class = "btn-sm btn-primary"))
    }
    card(card_header(if (h$jenis == "validasi") "Pra-penilaian Claude (saran, bukan nilai resmi)" else "Draf narasi Claude"), isi, helpText(biaya))
  })

  # ------------------------------------------------------------- Validasi
  tabel_validasi <- reactive({
    st <- status(); tm <- temuan()
    ringkas <- vapply(st$kode, function(k) { t <- tm[tm$kode == k & tm$tingkat != "info", ]
      if (nrow(t)) paste(sprintf("[%s] %s", t$tingkat, t$pesan), collapse = "\n") else "" }, character(1))
    d <- data.frame(Kode = st$kode, Area = ifelse(st$bagian == "Hasil", "Hasil", st$area), Komponen = st$komponen, Pertanyaan = st$pertanyaan,
                    Status = st$status, Jawaban = vapply(st$kode, ambil_isi, character(1), kol = "jawaban"),
                    Temuan = ringkas, PIC = vapply(st$kode, ambil_isi, character(1), kol = "pic"), stringsAsFactors = FALSE)
    if (input$v_status != "Semua") d <- d[d$Status == input$v_status, ]
    if (input$v_area != "Semua") d <- d[d$Area == input$v_area, ]
    d
  })
  output$tbl_validasi <- DT::renderDT({
    DT::datatable(tabel_validasi(), rownames = FALSE, selection = "single", filter = "top",
                  options = list(pageLength = 15, scrollX = TRUE, columnDefs = list(list(width = "35%", targets = 6)))) |>
      DT::formatStyle("Status", color = "white", fontWeight = "bold",
                      backgroundColor = DT::styleEqual(names(WARNA_STATUS), unname(WARNA_STATUS)))
  })
  observeEvent(input$tbl_validasi_rows_selected, {
    k <- tabel_validasi()$Kode[input$tbl_validasi_rows_selected]
    nav_select("nav", "Isi LKE"); buka_butir(k)
  })
  output$tbl_temuan <- DT::renderDT({
    tm <- temuan(); tm <- tm[order(match(tm$tingkat, c("merah", "kuning", "info")), tm$kode), ]
    DT::datatable(tm, rownames = FALSE, filter = "top", options = list(pageLength = 15))
  })

  # ------------------------------------------------------------- Analisis riwayat
  output$ui_analisis <- renderUI({
    rw <- riwayat()
    if (is.null(rw)) return(card(div(class = "alert alert-info",
      "Belum ada riwayat penilaian. Impor file LKE yang memuat jawaban & catatan TP PTN/TPSK/TPI di menu Pengaturan.")))
    tagList(
      card(card_header("Ringkasan"), uiOutput("an_ringkas")),
      layout_columns(col_widths = c(5, 7),
        card(card_header("Alasan penurunan yang paling sering (dari catatan TPI/TPSK)"), DT::DTOutput("an_alasan")),
        card(card_header("Penurunan per komponen"), DT::DTOutput("an_komponen"))),
      card(card_header("Butir yang diturunkan evaluator"), DT::DTOutput("an_butir"))
    )
  })
  analisis <- reactive({
    rw <- riwayat(); req(rw)
    x <- merge(BUTIR[BUTIR$pilihan %in% names(PETA_NILAI), c("kode", "grup", "komponen", "pertanyaan", "pilihan")], rw, by = "kode")
    x$ev <- ifelse(nzchar(x$jawaban_tpi), x$jawaban_tpi, ifelse(nzchar(x$jawaban_tpsk), x$jawaban_tpsk, x$jawaban_tp))
    x$evaluator <- ifelse(nzchar(x$jawaban_tpi), "TPI", ifelse(nzchar(x$jawaban_tpsk), "TPSK", "TP PTN"))
    x$catatan <- ifelse(nzchar(x$catatan_tpi), x$catatan_tpi, ifelse(nzchar(x$catatan_tpsk), x$catatan_tpsk, x$catatan_tp))
    x$n_unit <- mapply(nilai_pilihan, x$pilihan, x$jawaban_unit); x$n_ev <- mapply(nilai_pilihan, x$pilihan, x$ev)
    x$turun <- !is.na(x$n_unit) & !is.na(x$n_ev) & x$n_ev < x$n_unit
    x
  })
  output$an_ringkas <- renderUI({
    x <- analisis(); rw <- riwayat()
    s_unit <- hitung_skor(RB, stats::setNames(rw$jawaban_unit, rw$kode), FALSE)$ringkas
    s_tpi <- if (any(nzchar(rw$jawaban_tpi))) hitung_skor(RB, stats::setNames(rw$jawaban_tpi, rw$kode), FALSE)$ringkas else NULL
    tagList(
      p(sprintf("%d dari %d butir (%.0f%%) diberi nilai lebih rendah oleh evaluator dibanding klaim unit.",
                sum(x$turun), nrow(x), 100 * mean(x$turun))),
      p(sprintf("Pengungkit: klaim unit %.2f%s.", s_unit$skor[3],
                if (!is.null(s_tpi)) sprintf(" vs TPI %.2f (selisih %.2f poin)", s_tpi$skor[3], s_unit$skor[3] - s_tpi$skor[3]) else "")),
      p("Pola ini menunjukkan masalah utama: jawaban diklaim lebih tinggi daripada yang dapat dibuktikan dokumen. Gunakan daftar alasan di bawah sebagai bahan pelatihan PIC.")
    )
  })
  output$an_alasan <- DT::renderDT({
    x <- analisis(); x <- x[x$turun, ]
    kat <- c("Tidak berkala / baru sekali" = "berkala|rutin|periodik|secara konsisten",
             "Tindak lanjut tidak dibuktikan" = "tindak ?lanjut|ditindaklanjuti",
             "Inovasi tidak terbukti" = "inovasi",
             "Cakupan belum menyeluruh" = "seluruh|semua|sebagian|beberapa",
             "Dokumen tidak ada / tidak relevan" = "tidak relevan|tidak ada|tidak tersedia|tidak terdapat|belum ada|tidak menunjukkan|tidak menunjukan",
             "Dampak/hasil tidak terukur" = "dampak|terukur|efektivitas|manfaat",
             "Narasi/keterangan kurang" = "narasi|keterangan|deskripsi",
             "Perlu verifikasi substansi" = "verifikasi")
    n <- vapply(kat, function(p) sum(grepl(p, tolower(x$catatan), perl = TRUE)), integer(1))
    DT::datatable(data.frame(Alasan = names(kat), `Jumlah butir` = n, check.names = FALSE)[order(-n), ],
                  rownames = FALSE, options = list(dom = "t"))
  })
  output$an_komponen <- DT::renderDT({
    x <- analisis()
    a <- stats::aggregate(cbind(turun = x$turun, butir = 1) ~ grup + komponen, data = x, FUN = sum)
    a$persen <- round(100 * a$turun / a$butir)
    DT::datatable(a[order(a$grup), ], rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })
  output$an_butir <- DT::renderDT({
    x <- analisis(); x <- x[x$turun, ]
    DT::datatable(data.frame(Kode = x$kode, Pertanyaan = x$pertanyaan, Unit = x$jawaban_unit, `TP PTN` = x$jawaban_tp,
                             TPSK = x$jawaban_tpsk, TPI = x$jawaban_tpi, Catatan = x$catatan, check.names = FALSE),
                  rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })

  # ------------------------------------------------------------- Draf dokumen
  observeEvent(input$d_buat, {
    if (!nzchar(trimws(input$d_fakta))) { showNotification("Isi fakta kegiatan terlebih dahulu.", type = "error"); return() }
    b <- if (nzchar(input$d_butir)) BUTIR[BUTIR$kode == input$d_butir, ] else NULL
    withProgress(message = "Claude menyusun draf dokumen...", value = 0.3, {
      h <- claude_draf_dokumen(input$d_jenis, input$d_fakta, b)
    })
    if (!is.null(h$token_in)) catat_ai(con, input$d_butir %||% "", paste("dokumen:", input$d_jenis), h, tahun())
    draf_dok(h)
  })
  output$d_hasil <- renderUI({
    h <- draf_dok(); if (is.null(h)) return(helpText("Belum ada draf."))
    if (!isTRUE(h$ok)) return(div(class = "alert alert-danger", h$pesan))
    tagList(div(class = "alert alert-warning", "Periksa dan lengkapi setiap [PERLU DIISI] sebelum dokumen ditandatangani."),
            markdown(h$teks))
  })
  output$d_unduh <- downloadHandler(
    filename = function() paste0(gsub("\\W+", "_", input$d_jenis), "_", format(Sys.Date()), ".docx"),
    content = function(file) { h <- draf_dok(); req(isTRUE(h$ok)); simpan_dokumen_docx(h$teks, file) })

  # ------------------------------------------------------------- SOP
  hasil_sop <- eventReactive(input$s_periksa, {
    req(input$s_daftar)
    ds <- baca_daftar_sop(input$s_daftar$datapath)
    pdf <- input$s_pdf
    withProgress(message = "Memeriksa SOP...", {
      periksa_sop(ds, if (is.null(pdf)) character(0) else pdf$datapath, if (is.null(pdf)) character(0) else pdf$name)
    })
  })
  output$s_temuan <- DT::renderDT({
    t <- hasil_sop()$temuan; t <- t[order(match(t$tingkat, c("merah", "kuning"))), ]
    DT::datatable(t, rownames = FALSE, filter = "top", options = list(pageLength = 20)) |>
      DT::formatStyle("tingkat", color = "white", backgroundColor = DT::styleEqual(c("merah", "kuning"), c("#dc3545", "#e0a800")))
  })
  output$s_pdf_tbl <- DT::renderDT(DT::datatable(hasil_sop()$pdf, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)))

  # ------------------------------------------------------------- Ekspor
  output$e_excel <- downloadHandler(
    filename = function() sprintf("Isian_LKE_ZI_%d_%s.xlsx", tahun(), format(Sys.Date(), "%Y%m%d")),
    content = function(file) ekspor_lembar_isian(RB, isian(), status(), temuan(), skor(), file, tahun()))
  output$e_docx <- downloadHandler(
    filename = function() sprintf("Laporan_Validasi_LKE_%d_%s.docx", tahun(), format(Sys.Date(), "%Y%m%d")),
    content = function(file) laporan_validasi_docx(RB, isian(), status(), temuan(), skor(), file, tahun()))

  # ------------------------------------------------------------- Pengaturan
  observeEvent(input$p_effort, KONFIG$effort <<- input$p_effort, ignoreInit = TRUE)
  output$p_status_ai <- renderUI({
    if (claude_tersedia()) div(class = "alert alert-success", sprintf("Claude aktif \u00B7 model %s", KONFIG$model))
    else div(class = "alert alert-secondary", "Claude belum aktif: ANTHROPIC_API_KEY belum diatur di .Renviron. Fitur lain tetap berjalan.")
  })
  observeEvent(input$p_impor_isian, {
    req(input$p_file_isian)
    hasil <- tryCatch({
      df <- baca_isian_excel(input$p_file_isian$datapath, RB, tahun())
      df <- df[nzchar(df$jawaban) | nzchar(df$narasi) | nzchar(df$bukti), ]
      if (!isTRUE(input$p_timpa)) {
        sudah <- isian(); sudah <- sudah$kode[nzchar(sudah$jawaban) | nzchar(sudah$narasi)]
        df <- df[!(df$kode %in% sudah), ]
      }
      simpan_isian(con, df, tahun(), oleh = "impor", aksi = paste("impor", input$p_file_isian$name))
      nrow(df)
    }, error = function(e) conditionMessage(e))
    pemicu(pemicu() + 1)
    showNotification(if (is.numeric(hasil)) sprintf("%d baris isian diimpor.", hasil) else paste("Gagal:", hasil),
                     type = if (is.numeric(hasil)) "message" else "error")
  })
  observeEvent(input$p_impor_riwayat, {
    req(input$p_file_riwayat)
    hasil <- tryCatch({
      rw <- baca_riwayat(input$p_file_riwayat$datapath, RB, sumber = input$p_file_riwayat$name)
      dir.create(KONFIG$folder_lokal, showWarnings = FALSE, recursive = TRUE)
      utils::write.csv(rw, file_riwayat(), row.names = FALSE, fileEncoding = "UTF-8")
      riwayat(muat_riwayat()); "ok"
    }, error = function(e) conditionMessage(e), warning = function(w) conditionMessage(w))
    showNotification(if (hasil == "ok") "Riwayat penilaian diimpor." else paste("Perhatian:", hasil), type = if (hasil == "ok") "message" else "warning")
  })
  log_ai <- reactive({ pemicu(); hasil_ai(); draf_dok(); baca_log_ai(con, tahun()) })
  output$p_biaya <- renderText(sprintf("Total %d permintaan, estimasi biaya US$%.2f tahun %d.", nrow(log_ai()), sum(log_ai()$biaya), tahun()))
  output$p_log_ai <- DT::renderDT(DT::datatable(log_ai()[, c("waktu", "kode", "jenis", "model", "token_in", "token_out", "biaya")],
                                                rownames = FALSE, options = list(pageLength = 5)))
  output$p_jejak <- DT::renderDT({ pemicu(); DT::datatable(baca_jejak(con, tahun()), rownames = FALSE, options = list(pageLength = 10)) })
}

shinyApp(ui, server)

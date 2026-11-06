# =============================================================================
# 80_tampilan.R - Tema warna, gaya (CSS), dan elemen tampilan
# -----------------------------------------------------------------------------
# Tema "Samudra Integritas": latar gradasi biru tua -> teal, kartu putih
# melayang, aksen emas. Ganti warna di daftar WARNA untuk mengubah tema.
# =============================================================================

WARNA <- list(
  navy  = "#0B2545",   # biru tua utama
  biru  = "#134074",
  teal  = "#13838F",
  mint  = "#E6F4F1",
  emas  = "#F2A541",   # aksen
  latar = "#F4F8FB"    # latar panel samping
)

tema_aplikasi <- function() {
  bslib::bs_theme(
    version = 5,
    primary = WARNA$teal, secondary = WARNA$biru, success = "#2E9E6B",
    warning = WARNA$emas, danger = "#D64550", info = "#3A86C8",
    blue = WARNA$biru, teal = WARNA$teal, indigo = WARNA$navy, orange = WARNA$emas,
    base_font = bslib::font_collection(bslib::font_google("Plus Jakarta Sans", local = FALSE),
                                "Segoe UI", "Roboto", "sans-serif"),
    "border-radius" = "0.75rem"
  )
}

CSS_APLIKASI <- sprintf("
body {
  background:
    radial-gradient(circle at 12%% -10%%, rgba(58,134,200,.55) 0%%, transparent 45%%),
    radial-gradient(circle at 95%% 110%%, rgba(242,165,65,.30) 0%%, transparent 40%%),
    linear-gradient(160deg, %1$s 0%%, %2$s 45%%, %3$s 100%%);
  background-attachment: fixed;
  min-height: 100vh;
}
.navbar { background: rgba(11,37,69,.82) !important; backdrop-filter: blur(8px);
          box-shadow: 0 2px 14px rgba(0,0,0,.30); }
.navbar .navbar-brand { color: #fff !important; font-weight: 700; letter-spacing: .3px; }
.navbar .nav-link { color: rgba(255,255,255,.78) !important; }
.navbar .nav-link:hover { color: #fff !important; }
.navbar .nav-link.active { color: #fff !important; border-bottom: 3px solid %4$s; }
.navbar .unit-kerja { color: rgba(255,255,255,.65); }

/* Kartu melayang */
.card { border: 0; border-radius: 16px; box-shadow: 0 10px 28px rgba(5,20,40,.28); }
.card > .card-header { background: linear-gradient(90deg, %1$s, %3$s); color: #fff; font-weight: 600;
                       border-bottom: 0; border-radius: 16px 16px 0 0 !important; }
.card > .card-header .btn { color: #fff; border-color: rgba(255,255,255,.6); }
.card > .card-header .btn:hover { background: rgba(255,255,255,.15); }
.bslib-value-box { box-shadow: 0 10px 28px rgba(5,20,40,.35); }

/* Tata letak dengan panel samping */
.bslib-sidebar-layout { border: 0 !important; border-radius: 16px; background: transparent !important;
                        box-shadow: 0 10px 28px rgba(5,20,40,.28); }
.bslib-sidebar-layout > .sidebar { background: %5$s !important; border-right: 0 !important; }
.bslib-sidebar-layout > .main { background: rgba(255,255,255,.06); }
.bslib-sidebar-layout > .collapse-toggle { color: %1$s; }

/* Kode butir */
code.kode { background: %6$s; color: #0B5563; padding: 1px 7px; border-radius: 6px;
            font-weight: 700; font-size: .78rem; white-space: nowrap; }
.kode-besar { font-size: 1rem !important; padding: 3px 10px !important; }
.keterangan { background: %6$s; border-left: 5px solid %3$s; border-radius: 10px;
              padding: .75rem 1rem; margin: .25rem 0 1rem 0; }
.keterangan .label { font-size: .72rem; text-transform: uppercase; letter-spacing: .08em; color: %3$s; font-weight: 700; }

/* Dropdown per zona */
.accordion-zona .accordion-item { border: 0; margin-bottom: .45rem; border-radius: 12px !important; overflow: hidden;
                                  box-shadow: 0 1px 4px rgba(11,37,69,.12); }
.accordion-zona .accordion-button { background: #fff; font-weight: 600; font-size: .88rem; padding: .6rem .8rem; color: %1$s; }
.accordion-zona .accordion-button:not(.collapsed) { background: linear-gradient(90deg, %1$s, %2$s); color: #fff; box-shadow: none; }
.accordion-zona .accordion-button:not(.collapsed)::after { filter: invert(1) brightness(2); }
.accordion-zona .accordion-body { padding: .4rem .35rem .6rem .35rem; background: #fff; max-height: 52vh; overflow-y: auto; }
.judul-zona { display: flex; flex-direction: column; gap: 2px; width: 100%%; }
.hitung { display: flex; gap: 4px; }
.hitung span { font-size: .68rem; font-weight: 700; border-radius: 10px; padding: 0 7px; color: #fff; }
.sub-bagian { font-size: .7rem; font-weight: 800; text-transform: uppercase; letter-spacing: .08em;
              color: %3$s; margin: .5rem .4rem .15rem .4rem; }
.sub-komponen { font-size: .72rem; color: #5b6b7c; margin: .3rem .5rem .1rem .5rem; font-style: italic; }
a.item-butir { display: flex; gap: .45rem; align-items: flex-start; padding: .32rem .5rem; border-radius: 8px;
               color: #1b2a3a; text-decoration: none; font-size: .82rem; line-height: 1.25; }
a.item-butir:hover { background: #E3EEF7; }
a.item-butir.aktif { background: %1$s; color: #fff; }
a.item-butir.aktif code.kode { background: %4$s; color: %1$s; }
.titik { width: 10px; height: 10px; border-radius: 50%%; flex: none; margin-top: 4px; }

/* Tabel */
table.dataTable thead th { background: %1$s; color: #fff; }
table.dataTable tbody tr.selected td { box-shadow: inset 0 0 0 9999px rgba(19,131,143,.25) !important; }
.kriteria td { vertical-align: top; padding: 4px 8px; }
.accordion-button:not(.collapsed) { background: %6$s; color: %1$s; }
", WARNA$navy, WARNA$biru, WARNA$teal, WARNA$emas, WARNA$latar, WARNA$mint)

# JavaScript kecil: menandai butir aktif di daftar tanpa menggambar ulang daftar
JS_APLIKASI <- "
Shiny.addCustomMessageHandler('tandai_butir', function(kode) {
  document.querySelectorAll('a.item-butir').forEach(function(a) { a.classList.toggle('aktif', a.dataset.kode === kode); });
  var el = document.querySelector('a.item-butir[data-kode=\"' + kode + '\"]');
  if (el) setTimeout(function() { el.scrollIntoView({block: 'nearest', behavior: 'smooth'}); }, 350);
});
"

# Pertanyaan panjang -> dipotong pada batas kata; teks lengkap jadi keterangan
ringkas_teks <- function(x, n = 55) {
  x <- gsub("\\s+", " ", trimws(x))
  panjang <- nchar(x) > n
  potong <- sub("\\s+\\S*$", "", substr(x, 1, n))
  ifelse(panjang, paste0(potong, "\u2026"), x)
}

chip_kode <- function(kode, besar = FALSE) htmltools::tags$code(class = paste("kode", if (besar) "kode-besar"), kode)

# Versi HTML untuk tabel DT (kode + judul singkat, teks lengkap muncul saat disorot)
butir_html <- function(kode, pertanyaan, n = 55) {
  sprintf('<span title="%s"><code class="kode">%s</code> %s</span>',
          htmltools::htmlEscape(pertanyaan, attribute = TRUE), htmltools::htmlEscape(kode),
          htmltools::htmlEscape(ringkas_teks(pertanyaan, n)))
}

label_zona <- function(b) {
  if (b$bagian[1] == "Hasil") "Hasil" else sprintf("Zona %d", b$komponen_no[1])
}

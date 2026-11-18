// =============================================================================
// Bundel antarmuka dashboard LKE ZI
//   - anime.js : cincin progres kartu KPI dan kemunculan kartu/banner
//   - Motion   : angka KPI bergaya odometer, animasi masuk saat digulir
//                (inView), efek hover pegas, dan
//                penegasan saat isian menjadi bebas temuan
//   - Recharts : grafik bergaya shadcn/ui charts (komponen ChartContainer,
//                tooltip, dan legenda ditulis ulang di sini karena kode
//                shadcn diambil dari ui.shadcn.com yang tidak dapat diakses
//                saat pembangunan)
// Dibangun menjadi www/vendor/zi-ui.min.js dengan: cd js && npm run build
// =============================================================================
import { animate as anime, stagger, utils } from 'animejs';
import { animate as gerak, inView, hover } from 'motion';
import React, { useEffect, useRef, useState } from 'react';
import { createRoot } from 'react-dom/client';
import {
  BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, LabelList,
} from 'recharts';

const kurangiGerak = () => window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
const fmt = (n, desimal = 1) => new Intl.NumberFormat('id-ID', { maximumFractionDigits: desimal, minimumFractionDigits: 0 }).format(n);

// ----------------------------------------------------------------------------
// 1. Angka KPI bergaya odometer (Motion): setiap digit bergulir vertikal dari
//    nilai lama ke nilai baru; setelah selesai, isi elemen kembali menjadi
//    teks biasa sehingga dapat disalin dan dibaca pembaca layar.
// ----------------------------------------------------------------------------
const POLA_ANGKA = /-?\d+(?:\.\d+)?/g;
const TINGGI_DIGIT = 1.1; // em; harus sama dengan .odo-kolom di CSS

function urutanDigit(dari, ke, naik) {
  const hasil = [dari];
  let d = dari;
  while (d !== ke) { d = naik ? (d + 1) % 10 : (d + 9) % 10; hasil.push(d); }
  return hasil;
}

function buatSpan(kelas, teks) {
  const s = document.createElement('span');
  if (kelas) s.className = kelas;
  if (teks !== undefined) s.textContent = teks;
  return s;
}

function odometer(el, teksLama, teksBaru) {
  const baru = teksBaru.match(POLA_ANGKA) || [];
  if (!baru.length || kurangiGerak()) { el.textContent = teksBaru; return; }
  const lama = (teksLama || '').match(POLA_ANGKA) || [];
  const bagian = teksBaru.split(POLA_ANGKA);
  const tampil = buatSpan(''); tampil.setAttribute('aria-hidden', 'true');
  const kolom = [];
  bagian.forEach((teks, i) => {
    if (teks) tampil.append(teks);
    if (i >= baru.length) return;
    const sBaru = baru[i];
    const desimal = (sBaru.split('.')[1] || '').length;
    const vBaru = parseFloat(sBaru);
    const vLama = lama[i] !== undefined ? parseFloat(lama[i]) : 0;
    const naik = vBaru >= vLama;
    if (vBaru < 0) tampil.append('-');
    let [intB, desB = ''] = Math.abs(vBaru).toFixed(desimal).split('.');
    let [intL, desL = ''] = Math.abs(vLama).toFixed(desimal).split('.');
    const panjang = Math.max(intB.length, intL.length);
    intB = intB.padStart(panjang, '0'); intL = intL.padStart(panjang, '0');
    const cB = intB + (desimal ? `.${desB}` : '');
    const cL = intL + (desimal ? `.${desL}` : '');
    for (let k = 0; k < cB.length; k += 1) {
      if (cB[k] === '.') { tampil.append('.'); continue; }
      const a = Number(cL[k]); const b = Number(cB[k]);
      if (a === b) { tampil.append(cB[k]); continue; }
      const urut = urutanDigit(a, b, naik);
      const pita = buatSpan('odo-pita');
      (naik ? urut : [...urut].reverse()).forEach((d) => pita.append(buatSpan('', String(d))));
      const wadah = buatSpan('odo-kolom'); wadah.append(pita); tampil.append(wadah);
      kolom.push({ pita, langkah: urut.length - 1, naik });
    }
  });
  if (!kolom.length) { el.textContent = teksBaru; return; }
  const token = (el._odometer || 0) + 1;
  el._odometer = token;
  el.textContent = '';
  el.append(buatSpan('visually-hidden', teksBaru), tampil);
  let selesai = 0;
  kolom.forEach((c, j) => {
    const jauh = c.langkah * TINGGI_DIGIT;
    const [dari, ke] = c.naik ? [0, -jauh] : [-jauh, 0];
    c.pita.style.transform = `translateY(${dari}em)`;
    gerak(c.pita, { transform: [`translateY(${dari}em)`, `translateY(${ke}em)`] },
      { duration: 0.55 + Math.min(c.langkah, 9) * 0.06, delay: (kolom.length - 1 - j) * 0.04, ease: [0.22, 1, 0.36, 1] })
      .then(() => {
        selesai += 1;
        if (selesai === kolom.length && el._odometer === token) el.textContent = teksBaru;
      });
  });
}

function pasangKpi() {
  if (!window.jQuery) return;
  window.jQuery(document).on('shiny:value', (e) => {
    const el = e.target;
    if (!el || !el.id || !el.id.startsWith('vb_') || typeof e.value !== 'string') return;
    const lama = el.dataset.nilai || '';
    el.dataset.nilai = e.value;
    // Biarkan Shiny menulis nilainya terlebih dahulu, lalu animasikan.
    setTimeout(() => odometer(el, lama, e.value), 0);
  });
}

// anime.js: cincin progres di sekeliling ikon kartu KPI (nilai 0-1 dari server)
function pasangCincin() {
  if (!window.Shiny || !window.Shiny.addCustomMessageHandler) return;
  window.Shiny.addCustomMessageHandler('zi_cincin', (pesan) => {
    Object.entries(pesan || {}).forEach(([id, rasio], urutan) => {
      const keluaran = document.getElementById(id);
      const kartu = keluaran && keluaran.closest('.kpi-kartu');
      const isi = kartu && kartu.querySelector('.kpi-cincin .isi');
      if (!isi) return;
      const nilai = Math.max(0, Math.min(1, Number(rasio) || 0));
      const tujuan = 100 - nilai * 100;
      kartu.dataset.cincin = String(Math.round(nilai * 100));
      if (kurangiGerak()) { isi.style.strokeDashoffset = String(tujuan); return; }
      if (isi._animasi) isi._animasi.pause();
      isi._animasi = anime(isi, { strokeDashoffset: tujuan, duration: 1300, delay: 250 + urutan * 90, ease: 'outExpo' });
    });
  });
}

// anime.js: kartu KPI dan teks banner muncul berurutan saat halaman dimuat
function animasiMasuk() {
  if (kurangiGerak()) return;
  const kartu = document.querySelectorAll('.kpi-kartu');
  if (kartu.length) {
    anime(kartu, {
      opacity: { from: 0, to: 1 }, translateY: { from: 18, to: 0 },
      delay: stagger(90), duration: 700, ease: 'outCubic',
      onComplete: (animasi) => utils.cleanInlineStyles(animasi),
    });
  }
  const teks = document.querySelectorAll('#banner-situs.pakai-bawaan .keterangan-banner > *');
  if (teks.length) {
    anime(teks, {
      opacity: { from: 0, to: 1 }, translateX: { from: -16, to: 0 },
      delay: stagger(120, { start: 150 }), duration: 650, ease: 'outQuart',
      onComplete: (animasi) => utils.cleanInlineStyles(animasi),
    });
  }
}

// ----------------------------------------------------------------------------
// 2. Motion: kartu muncul saat digulir, hover pegas, penegasan bebas temuan
// ----------------------------------------------------------------------------
function animasiGulir() {
  if (kurangiGerak()) return;
  const lipat = window.innerHeight;
  document.querySelectorAll('.tab-content .card').forEach((kartu) => {
    if (kartu.closest('#ui_form') || kartu.getBoundingClientRect().top < lipat) return;
    kartu.style.opacity = '0';
    inView(kartu, () => {
      gerak(kartu, { opacity: [0, 1], y: [24, 0] }, { duration: 0.5, ease: [0.22, 1, 0.36, 1] });
    }, { amount: 0.15 });
  });
}

function animasiHover() {
  if (kurangiGerak()) return;
  const pegas = { type: 'spring', stiffness: 320, damping: 22 };
  hover('.kpi-kartu', (el) => {
    gerak(el, { y: -4, scale: 1.015 }, pegas);
    return () => gerak(el, { y: 0, scale: 1 }, pegas);
  });
}

function pasangPenegasan() {
  if (!window.jQuery) return;
  let adaTemuan = true;
  window.jQuery(document).on('shiny:value', (e) => {
    if (!e.target || e.target.id !== 'ui_temuan_live') return;
    setTimeout(() => {
      const sukses = e.target.querySelector('.alert-success');
      if (sukses && adaTemuan && !kurangiGerak()) {
        gerak(sukses, { opacity: [0, 1], scale: [0.94, 1] }, { type: 'spring', bounce: 0.45, duration: 0.6 });
      }
      adaTemuan = !sukses;
    }, 0);
  });
  window.jQuery(document).on('shiny:value', (e) => {
    if (!e.target || e.target.id !== 'ui_hasil_ai' || kurangiGerak()) return;
    setTimeout(() => {
      const kartu = e.target.querySelector('.card');
      if (kartu) gerak(kartu, { opacity: [0, 1], y: [16, 0] }, { type: 'spring', bounce: 0.25, duration: 0.6 });
    }, 0);
  });
}

// ----------------------------------------------------------------------------
// 2b. Motion: penanda yang meluncur (ribbon menu dan filter status pil)
// ----------------------------------------------------------------------------
const PEGAS_PENANDA = { type: 'spring', stiffness: 420, damping: 34, mass: 0.9 };

function geserPenanda(penanda, kotak, langsung) {
  const tujuan = { x: kotak.x, y: kotak.y, width: `${kotak.w}px`, height: `${kotak.h}px` };
  gerak(penanda, tujuan, (langsung || kurangiGerak()) ? { duration: 0 } : PEGAS_PENANDA);
}

function pasangPenandaRibbon() {
  const ul = document.getElementById('nav');
  if (!ul) return;
  const penanda = buatSpan('penanda-ribbon');
  penanda.setAttribute('aria-hidden', 'true');
  ul.prepend(penanda);
  ul.classList.add('ada-penanda');
  let langsung = true;
  const sasaran = () => {
    const aktif = ul.querySelector(':scope > li > a.nav-link.active:not(.dropdown-toggle)');
    if (aktif) return aktif;
    const sub = ul.querySelector('.dropdown-item.active');
    return sub ? sub.closest('.dropdown').querySelector('.dropdown-toggle') : null;
  };
  const tempatkan = () => {
    const a = sasaran();
    if (!a || getComputedStyle(penanda).display === 'none') { langsung = true; return; }
    const ru = ul.getBoundingClientRect(); const ra = a.getBoundingClientRect();
    geserPenanda(penanda, { x: ra.left - ru.left + ul.scrollLeft, y: ra.top - ru.top, w: ra.width, h: ra.height }, langsung);
    langsung = false;
  };
  document.addEventListener('shown.bs.tab', () => requestAnimationFrame(tempatkan));
  new ResizeObserver(() => { langsung = true; tempatkan(); }).observe(ul);
  if (document.fonts && document.fonts.ready) document.fonts.ready.then(() => { langsung = true; tempatkan(); });
  tempatkan();
}

function pasangPilStatus() {
  const pasang = (wadah) => {
    const grup = wadah.querySelector('.shiny-options-group');
    if (!grup) return;
    let penanda = grup.querySelector(':scope > .pil-penanda');
    const baru = !penanda;
    if (baru) {
      penanda = buatSpan('pil-penanda');
      penanda.setAttribute('aria-hidden', 'true');
      grup.prepend(penanda);
    }
    const tempatkan = (langsung) => {
      const lbl = grup.querySelector('input:checked');
      const label = lbl && lbl.closest('label');
      if (!label || !label.offsetWidth) return;
      geserPenanda(penanda, { x: label.offsetLeft, y: label.offsetTop, w: label.offsetWidth, h: label.offsetHeight }, langsung);
    };
    wadah.classList.add('siap');
    if (!grup._pilTerpasang) {
      grup._pilTerpasang = true;
      grup.addEventListener('change', () => tempatkan(false));
      new ResizeObserver(() => tempatkan(true)).observe(grup);
    }
    tempatkan(true);
  };
  document.querySelectorAll('.pil-status').forEach((wadah) => {
    pasang(wadah);
    // updateRadioButtons menulis ulang isi grup: pasang kembali penandanya.
    new MutationObserver(() => {
      const grup = wadah.querySelector('.shiny-options-group');
      if (grup && !grup.querySelector(':scope > .pil-penanda')) { grup._pilTerpasang = false; pasang(wadah); }
    }).observe(wadah, { childList: true, subtree: true });
  });
}

// Motion: transisi singkat saat berpindah tab. Halaman baru memudar masuk dan
// bergeser naik 6 px; halaman lama langsung disembunyikan agar tidak ada jeda.
// Durasi diatur dari R (KONFIG$transisi_tab_ms); 0 mematikan transisi.
function pasangTransisiTab() {
  const meta = document.querySelector('meta[name="zi-transisi-tab"]');
  const durasi = Math.max(0, Number(meta ? meta.content : 200) || 0) / 1000;
  if (!durasi) return;
  document.addEventListener('shown.bs.tab', (e) => {
    if (kurangiGerak()) return;
    const tautan = e.target;
    const sel = tautan && (tautan.getAttribute('data-bs-target') || tautan.getAttribute('href'));
    if (!sel || sel.charAt(0) !== '#') return;
    const panel = document.getElementById(sel.slice(1));
    if (!panel || !panel.classList.contains('tab-pane')) return;
    if (panel._transisi) panel._transisi.stop();
    panel._transisi = gerak(panel, { opacity: [0, 1], y: [6, 0] }, { duration: durasi, ease: [0.22, 1, 0.36, 1] });
    panel._transisi.then(() => { panel.style.transform = ''; panel.style.opacity = ''; });
  });
}

// ----------------------------------------------------------------------------
// 3. Grafik bergaya shadcn/ui charts di atas Recharts
// ----------------------------------------------------------------------------
function useLebar(ref) {
  const [lebar, setLebar] = useState(0);
  useEffect(() => {
    if (!ref.current) return undefined;
    const ro = new ResizeObserver(([e]) => setLebar(e.contentRect.width));
    ro.observe(ref.current);
    return () => ro.disconnect();
  }, [ref]);
  return lebar;
}

function TooltipShadcn({ active, payload, label, satuan, desimal }) {
  if (!active || !payload || !payload.length) return null;
  const judul = (payload[0].payload && payload[0].payload.label) || label;
  return (
    <div className="zi-tooltip">
      <div className="zi-tooltip-judul">{judul}</div>
      {payload.map((p) => (
        <div className="zi-tooltip-baris" key={p.dataKey}>
          <span className="zi-tooltip-indikator" style={{ background: p.color }} />
          <span className="zi-tooltip-label">{p.name}</span>
          <span className="zi-tooltip-nilai">{fmt(p.value, desimal)}{satuan}</span>
        </div>
      ))}
    </div>
  );
}

function LegendaShadcn({ seri }) {
  return (
    <div className="zi-legenda" role="list">
      {seri.map((s) => (
        <span className="zi-legenda-item" role="listitem" key={s.key}>
          <span className="zi-legenda-titik" style={{ background: s.warna }} />{s.label}
        </span>
      ))}
    </div>
  );
}

// Shiny mengirim data frame per kolom; ubah menjadi baris untuk Recharts.
function keBaris(data) {
  if (Array.isArray(data)) return data;
  const kolom = Object.keys(data || {});
  const n = kolom.length ? data[kolom[0]].length : 0;
  return Array.from({ length: n }, (_, i) => Object.fromEntries(kolom.map((k) => [k, data[k][i]])));
}

function GrafikZi({ spek }) {
  const ref = useRef(null);
  const lebar = useLebar(ref);
  const baris = keBaris(spek.data);
  const seri = spek.seri || [];
  const tumpuk = !!spek.tumpuk;
  const sempit = lebar > 0 && lebar < (spek.batas_sempit || 560);
  const kunciLabel = sempit && baris.length && baris[0].pendek !== undefined ? 'pendek' : 'label';
  const tinggiBaris = tumpuk ? 30 : 9 * seri.length + 16;
  const tinggi = Math.max(spek.tinggi_min || 220, baris.length * tinggiBaris + 40);
  const lebarSumbu = sempit ? 92 : (spek.lebar_label || 210);
  const animasi = !kurangiGerak();
  const satuan = spek.satuan || '';
  const desimal = spek.desimal ?? 1;
  const ringkasan = `${spek.judul || 'Grafik'}: ${baris.map((b) => `${b.label} ${seri.map((s) => `${s.label} ${fmt(b[s.key] ?? 0, desimal)}${satuan}`).join(', ')}`).join('; ')}`;

  return (
    <div ref={ref} className="zi-grafik" role="img" aria-label={ringkasan}>
      <LegendaShadcn seri={seri} />
      <div style={{ width: '100%', height: tinggi }}>
        <ResponsiveContainer width="100%" height="100%">
          <BarChart data={baris} layout="vertical" margin={{ top: 4, right: spek.label_nilai ? 44 : 12, bottom: 4, left: 4 }}
                    barCategoryGap={tumpuk ? '22%' : '18%'} barGap={2} accessibilityLayer>
            <CartesianGrid horizontal={false} strokeDasharray="3 3" stroke="var(--zi-grid)" />
            <XAxis type="number" domain={spek.domain || [0, 'auto']} tickLine={false} axisLine={false}
                   tickMargin={6} tick={{ fill: 'var(--zi-teks-redup)', fontSize: 11 }}
                   tickFormatter={(v) => `${fmt(v, 0)}${satuan}`} />
            <YAxis type="category" dataKey={kunciLabel} width={lebarSumbu} tickLine={false} axisLine={false}
                   tick={{ fill: 'var(--zi-teks)', fontSize: sempit ? 11 : 12 }} interval={0} />
            <Tooltip cursor={{ fill: 'var(--zi-kursor)' }} content={<TooltipShadcn satuan={satuan} desimal={desimal} />}
                     isAnimationActive={false} />
            {seri.map((s, i) => {
              const terakhir = i === seri.length - 1;
              const radius = tumpuk ? (terakhir ? [0, 5, 5, 0] : [0, 0, 0, 0]) : [0, 5, 5, 0];
              return (
                <Bar key={s.key} dataKey={s.key} name={s.label} fill={s.warna} radius={radius}
                     stackId={tumpuk ? 'a' : undefined} isAnimationActive={animasi}
                     animationDuration={800} animationBegin={i * 120} animationEasing="ease-out">
                  {spek.label_nilai && (!tumpuk || terakhir) && (
                    <LabelList dataKey={s.key} position="right" offset={6}
                               formatter={(v) => (v ? `${fmt(v, desimal)}${satuan}` : '')}
                               style={{ fill: 'var(--zi-teks-redup)', fontSize: 11 }} />
                  )}
                </Bar>
              );
            })}
          </BarChart>
        </ResponsiveContainer>
      </div>
      {spek.catatan && <div className="zi-grafik-catatan">{spek.catatan}</div>}
    </div>
  );
}

function daftarkanOutputShiny() {
  if (!window.Shiny || !window.Shiny.OutputBinding) return;
  const $ = window.jQuery;
  const binding = new window.Shiny.OutputBinding();
  $.extend(binding, {
    find: (scope) => $(scope).find('.zi-chart'),
    renderValue: (el, spek) => {
      if (!spek) return;
      if (!el._akar) el._akar = createRoot(el);
      el._akar.render(<GrafikZi spek={spek} />);
    },
    renderError: (el, err) => {
      if (!el._akar) el._akar = createRoot(el);
      el._akar.render(<div className="zi-grafik-galat">{err.message}</div>);
    },
    clearError: () => {},
  });
  window.Shiny.outputBindings.register(binding, 'zi.grafik');
}

// ----------------------------------------------------------------------------
daftarkanOutputShiny();
pasangKpi();
pasangCincin();
pasangPenegasan();
const mulai = () => { pasangPenandaRibbon(); pasangPilStatus(); pasangTransisiTab(); animasiMasuk(); animasiHover(); setTimeout(animasiGulir, 300); };
if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', mulai); else mulai();

window.ZiUI = { odometer, versi: '1.3.0' };

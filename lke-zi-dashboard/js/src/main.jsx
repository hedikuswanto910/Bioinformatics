// =============================================================================
// Bundel antarmuka dashboard LKE ZI
//   - anime.js : animasi angka KPI dan kemunculan kartu/banner
//   - Motion   : animasi masuk saat digulir (inView), efek hover pegas, dan
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
// 1. anime.js: angka KPI dihitung naik/turun ke nilai baru
// ----------------------------------------------------------------------------
const POLA_ANGKA = /-?\d+(?:\.\d+)?/g;

function animasiAngka(el, teksLama, teksBaru) {
  const baru = teksBaru.match(POLA_ANGKA) || [];
  const lama = (teksLama || '').match(POLA_ANGKA) || [];
  if (!baru.length || kurangiGerak()) { el.textContent = teksBaru; return; }
  const bagian = teksBaru.split(POLA_ANGKA);
  const desimal = baru.map((s) => (s.split('.')[1] || '').length);
  const awal = baru.map((s, i) => (lama[i] !== undefined ? parseFloat(lama[i]) : 0));
  const akhir = baru.map(parseFloat);
  const status = { t: 0 };
  if (el._animasiAngka) el._animasiAngka.pause();
  el._animasiAngka = anime(status, {
    t: 1, duration: 900, ease: 'outExpo',
    onUpdate: () => {
      el.textContent = bagian.map((b, i) => b + (i < akhir.length
        ? (awal[i] + (akhir[i] - awal[i]) * status.t).toFixed(desimal[i]) : '')).join('');
    },
    onComplete: () => { el.textContent = teksBaru; },
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
    setTimeout(() => animasiAngka(el, lama, e.value), 0);
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
pasangPenegasan();
const mulai = () => { animasiMasuk(); animasiHover(); setTimeout(animasiGulir, 300); };
if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', mulai); else mulai();

window.ZiUI = { animasiAngka, versi: '1.0.0' };

// Guía de compra · Honda Jazz 2016 — checklists, 3D hotspots, MOT chart, price ladder.
const STORE = 'guia-jazz:v1';
const state = (() => {
  try { return { checked: {}, open: {}, notes: '', ...JSON.parse(localStorage.getItem(STORE) || '{}') }; }
  catch { return { checked: {}, open: {}, notes: '' }; }
})();
const save = () => { try { localStorage.setItem(STORE, JSON.stringify(state)); } catch {} };
const $ = (s, r = document) => r.querySelector(s);
const el = (tag, attrs = {}, html = '') => { const e = document.createElement(tag); for (const [k, v] of Object.entries(attrs)) e.setAttribute(k, v); if (html) e.innerHTML = html; return e; };

function toast(msg) {
  const t = $('#toast'); t.textContent = msg; t.classList.add('on');
  clearTimeout(toast.t); toast.t = setTimeout(() => t.classList.remove('on'), 1600);
}

// ---------------------------------------------------------------- checklists
const K = true; // "clave": comes from this car's own history
const LISTS = [
  { id: 'llamada', emoji: '📞', title: 'Antes de ir: la llamada', sub: '(020) 7768 8031', items: [
    ['¿Sigue disponible?', 'Y pedí que no lo arranquen antes de que llegues (motor frío).'],
    ['¿Se puede hacer prueba de manejo?', '¿Con qué seguro? ¿Qué licencia y edad piden?'],
    ['¿Tienen el V5C y el libro de service ahí?'],
    ['¿Le cambiaron alguna vez el aceite de la CVT?', '¿Figura en el libro? Si no, pedí que lo incluyan.', K],
    ['¿Hicieron algo en frenos traseros o cubiertas desde el MOT de enero?', 'El MOT 2024 marcó el freno trasero derecho.', K],
    ['¿El MOT nuevo de 12 meses ya está hecho o se hace al comprar?'],
    ['¿Aceptan tarjeta de crédito, aunque sea una parte?', 'Para tener la protección Section 75.'],
  ]},
  { id: 'llevar', emoji: '🎒', title: 'Qué llevar', items: [
    ['Licencia de conducir física'],
    ['Celular cargado', 'Linterna para mirar abajo y en los pasarruedas; sacá fotos de todo.'],
    ['Una moneda de 20p', 'Para medir las cubiertas.'],
    ['Papel o guantes', 'Para la varilla del aceite.'],
    ['Seguro listo para activar', 'Cotizado de antemano con la patente AF16 EHR.'],
  ]},
  { id: 'papeles', emoji: '📄', title: 'Papeles', items: [
    ['El VIN del V5C coincide con el del auto', 'Base del parabrisas y etiqueta en el marco de la puerta.'],
    ['V5C: 2 dueños anteriores, sin "imported"', 'Y que el color figure como azul.'],
    ['Libro de service con sellos Honda', 'Fechas y millas coherentes; el último a 92.124 millas.', K],
    ['Informe HPI con copia', 'Sin finanzas pendientes, robo ni siniestro.'],
    ['Las 2 llaves abren y cierran a distancia'],
    ['Garantía de 6 meses por escrito', 'Quién la da, qué cubre y el tope por reclamo.'],
    ['Recalls pendientes', 'Chequear en gov.uk o en un concesionario Honda.'],
  ]},
  { id: 'exterior', emoji: '🚗', title: 'Exterior y carrocería', items: [
    ['Mirá los reflejos agachado, de punta a punta', 'Si los reflejos se ondulan, hubo chapa y pintura.'],
    ['El tono es igual en todos los paneles', 'Ojo con pintura sobre gomas o burletes.'],
    ['Las separaciones entre paneles son parejas', 'Capot, puertas y portón.'],
    ['Sin óxido', 'Bordes de los pasarruedas, zócalos y parte baja del portón.'],
    ['Parabrisas sin piedrazos en la zona del conductor'],
    ['Funcionan todas las luces', 'Posición, bajas, altas, giros, freno (pedí que alguien mire), marcha atrás y antiniebla.'],
    ['Limpiaparabrisas y sapito'],
    ['Llantas sin raspones de cordón', 'Los golpes fuertes desalinean.'],
  ]},
  { id: 'ruedas', emoji: '🛞', title: 'Cubiertas y frenos', items: [
    ['Las 4 cubiertas tienen ≥ 3 mm', 'Prueba de la moneda de 20p.', K],
    ['Sin bordes gastados ni grietas en el costado', 'El MOT lo marcó en 2022–2024.', K],
    ['DOT de menos de 6 años', 'Últimos 4 números del costado: semana + año.'],
    ['Misma marca y modelo en cada eje'],
    ['Disco trasero derecho sin labio, surcos ni óxido grueso', 'Miralo por entre los rayos de la llanta.', K],
    ['Kit de inflado o rueda de auxilio', 'Y la llave de seguridad de las llantas, si tiene.'],
  ]},
  { id: 'motor', emoji: '🔧', title: 'Motor en frío', items: [
    ['El motor está frío al tocarlo', 'Si está tibio, lo arrancaron antes.'],
    ['Aceite entre las marcas y color ámbar o marrón', 'Sin "mayonesa" en la tapa del aceite.'],
    ['Refrigerante en nivel y de color limpio'],
    ['Sin pérdidas ni manchas', 'Abajo y alrededor del motor.'],
    ['Arranque en frío limpio', 'Sin golpeteos y con ralentí parejo.'],
    ['Escape sin humo', 'Ni azul (aceite) ni blanco espeso que no se va.'],
    ['Las luces del tablero se apagan', 'A los pocos segundos de arrancar.'],
  ]},
  { id: 'interior', emoji: '🪑', title: 'Interior y electrónica', items: [
    ['El tablero marca ~95.300 millas', 'Tiene que coincidir con el aviso y el MOT.'],
    ['El aire acondicionado enfría en 1–2 minutos'],
    ['Calefacción y desempañador trasero'],
    ['Levantavidrios, espejos y cierre centralizado'],
    ['Sensores de estacionamiento delanteros y traseros'],
    ['Bluetooth, radio y USB'],
    ['Los cinturones traseros salen y enrollan bien', 'Tuvo sillas de bebé varios años.', K],
    ['Los asientos traseros se rebaten y se levantan'],
    ['Baúl con alfombra seca y sin olor a humedad'],
    ['Desgaste de volante y pedales acorde a ~95k millas'],
  ]},
  { id: 'manejo', emoji: '🛣️', title: 'Prueba de manejo', items: [
    ['Sale desde parado sin tirones'],
    ['Sin temblor de la CVT a 5–20 mph', 'Acelerá poquito y soltá varias veces: es la falla típica.', K],
    ['Aceleración fuerte sin patinar ni zumbido metálico'],
    ['Frenada firme desde ~40 mph sin vibración', 'Frena recto y no vibra el pedal ni el volante.', K],
    ['No tira hacia un lado', 'Volante derecho en recta.'],
    ['Sin golpes de suspensión', 'En badenes y baches.'],
    ['A 50–60 mph, sin zumbidos ni vibraciones'],
    ['El freno de mano retiene en pendiente'],
    ['Marcha atrás y dirección a tope sin ruidos'],
  ]},
  { id: 'despues', emoji: '🔍', title: 'Después de manejar', items: [
    ['Sin goteos nuevos debajo del auto'],
    ['Sin olor a quemado ni a refrigerante'],
    ['Ninguna luz de falla en el tablero'],
    ['El ventilador del radiador arranca', 'Si el motor se calentó.'],
  ]},
  { id: 'cierre', emoji: '🤝', title: 'Cierre y pago', items: [
    ['Precio final acordado todo incluido', 'La admin fee ya está dentro de los £6.995.'],
    ['Los extras acordados figuran en la factura'],
    ['Al menos £100 pagados con tarjeta de crédito', 'Protección Section 75.'],
    ['Te dieron todo', 'V5C/2 (la parte verde), factura, certificado MOT, garantía, 2 llaves y libro de service.'],
    ['Seguro activo antes de salir'],
    ['Patente (VED) pagada online', 'Con el código de 12 dígitos del V5C/2.'],
  ]},
];
const allIds = [];
LISTS.forEach((l) => l.items.forEach((_, i) => allIds.push(`${l.id}.${i}`)));

function renderChecklists() {
  const root = $('#checklists');
  root.innerHTML = '';
  for (const l of LISTS) {
    const d = el('details', { class: 'cl', id: `cl-${l.id}` });
    if (state.open[l.id]) d.open = true;
    d.innerHTML = `<summary><span class="emoji" aria-hidden="true">${l.emoji}</span><span class="t"><b>${l.title}</b>${l.sub ? `<small>${l.sub}</small>` : ''}<span class="bar"><i></i></span></span><span class="count"></span><svg class="chev" width="10" height="16" viewBox="0 0 10 16" aria-hidden="true"><path d="M2 2l6 6-6 6" stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round"/></svg></summary>`;
    const ul = el('ul', { class: 'items' });
    l.items.forEach(([text, detail, key], i) => {
      const id = `${l.id}.${i}`;
      const li = el('li', { class: 'item' });
      const cb = el('input', { type: 'checkbox', id: `c-${id}` });
      cb.checked = !!state.checked[id];
      li.classList.toggle('done', cb.checked);
      cb.addEventListener('change', () => {
        state.checked[id] = cb.checked;
        if (!cb.checked) delete state.checked[id];
        li.classList.toggle('done', cb.checked);
        save(); updateProgress();
        if (cb.checked && navigator.vibrate) navigator.vibrate(8);
      });
      const lab = el('label', { for: `c-${id}` }, `<span>${text}${key ? '<span class="flag">CLAVE</span>' : ''}</span>${detail ? `<span class="d">${detail}</span>` : ''}`);
      li.append(cb, lab);
      ul.appendChild(li);
    });
    d.appendChild(ul);
    d.addEventListener('toggle', () => { state.open[l.id] = d.open; save(); });
    root.appendChild(d);
  }
  updateProgress();
}

function updateProgress() {
  let done = 0;
  for (const l of LISTS) {
    const n = l.items.length;
    const c = l.items.filter((_, i) => state.checked[`${l.id}.${i}`]).length;
    done += c;
    const d = $(`#cl-${l.id}`);
    if (d) {
      $('.count', d).textContent = `${c}/${n}`;
      $('.bar i', d).style.width = `${(c / n) * 100}%`;
    }
  }
  const total = allIds.length;
  $('#progressText').textContent = `${done}/${total}`;
  const C = 2 * Math.PI * 9;
  $('#ringVal').style.strokeDashoffset = String(C * (1 - done / total));
}

$('#resetBtn').addEventListener('click', () => {
  if (!confirm('¿Borrar todas las marcas del checklist?')) return;
  state.checked = {}; save(); renderChecklists(); toast('Checklist reiniciado');
});

const notes = $('#notes');
notes.value = state.notes || '';
notes.addEventListener('input', () => { state.notes = notes.value; save(); });

function openList(id) {
  const d = $(`#cl-${id}`);
  if (!d) return;
  d.open = true; state.open[id] = true; save();
  d.scrollIntoView({ behavior: matchMedia('(prefers-reduced-motion: reduce)').matches ? 'auto' : 'smooth', block: 'start' });
}

// ---------------------------------------------------------------- hotspots
const HOTSPOTS = [
  { id: 'cubiertas', title: 'Cubiertas', list: 'ruedas', pos: [1.26, 0.31, 0.88], normal: [0.35, 0, 1],
    checks: ['Profundidad con la moneda de 20p (ideal ≥ 3 mm).', 'Bordes gastados o grietas: el MOT los marcó en 2022–2024.', 'Fecha DOT: si tienen más de 6 años, pedí cambio.'] },
  { id: 'freno', title: 'Freno trasero derecho', list: 'ruedas', pos: [-1.27, 0.36, 0.88], normal: [-0.25, 0, 1],
    checks: ['MOT 2024: "service brake fluctuating" de este lado.', 'Mirá el disco entre los rayos: sin labio en el borde ni surcos.', 'En la prueba, frená firme: sin vibración en pedal ni volante.'] },
  { id: 'motor', title: 'Motor y niveles', list: 'motor', pos: [1.72, 0.86, 0.15], normal: [0.6, 1, 0.1],
    checks: ['Tocá el motor: tiene que estar frío.', 'Aceite entre marcas; sin "mayonesa" en la tapa.', 'Refrigerante en nivel; sin pérdidas abajo.'] },
  { id: 'vin', title: 'Parabrisas y VIN', list: 'papeles', pos: [0.78, 1.24, -0.45], normal: [0.7, 0.8, -0.4],
    checks: ['El VIN de la base del parabrisas tiene que coincidir con el V5C.', 'Piedrazos en la zona del conductor fallan el MOT.'] },
  { id: 'interior', title: 'Interior y cinturones', list: 'interior', pos: [-0.55, 1.22, 0.78], normal: [0, 0.25, 1],
    checks: ['Tuvo sillas de bebé: probá que los cinturones traseros enrollen bien.', 'Aire acondicionado, sensores, levantavidrios y Bluetooth.', 'Que el tablero marque ~95.300 millas.'] },
  { id: 'baul', title: 'Baúl y asientos', list: 'interior', pos: [-1.99, 0.9, 0.0], normal: [-1, 0, 0],
    checks: ['Alfombra seca y sin olor a humedad.', 'Que estén el kit de inflado o la auxilio.', 'Rebatí y levantá los asientos traseros.'] },
  { id: 'escape', title: 'Escape', list: 'motor', pos: [-1.98, 0.34, -0.45], normal: [-1, -0.1, -0.2],
    checks: ['En el arranque en frío: sin humo azul ni blanco espeso.', 'Sin ruidos metálicos ni pérdidas.'] },
  { id: 'luces', title: 'Luces', list: 'exterior', pos: [1.87, 0.76, -0.55], normal: [1, 0, -0.45],
    checks: ['Todas: posición, bajas, altas, giros, freno, marcha atrás y antiniebla.', 'Ópticas sin humedad adentro.'] },
  { id: 'pintura', title: 'Carrocería y pintura', list: 'exterior', pos: [0.35, 0.72, -0.84], normal: [0, 0, -1],
    checks: ['Reflejos parejos y mismo tono en todos los paneles.', 'Separaciones parejas en puertas, capot y portón.'] },
  { id: 'bajos', title: 'Bajos y zócalos', list: 'exterior', pos: [-0.3, 0.32, -0.84], normal: [0, -0.4, -1],
    checks: ['Con la linterna: óxido en zócalos y pasarruedas.', 'Golpes en el piso o en el escape.'] },
];

const panel = $('#spotPanel');
function showSpot(id) {
  const i = HOTSPOTS.findIndex((h) => h.id === id);
  const h = HOTSPOTS[i];
  if (!h) return;
  panel.innerHTML = `<h3><span class="num">${i + 1}</span>${h.title}</h3><ul>${h.checks.map((c) => `<li>${c}</li>`).join('')}</ul><button class="linkbtn" type="button" data-list="${h.list}">Ir al checklist →</button>`;
  $('.linkbtn', panel).addEventListener('click', () => openList(h.list));
  document.querySelectorAll('#spotList .seg').forEach((b) => b.setAttribute('aria-pressed', String(b.dataset.id === id)));
  scrollSpotIntoView(id);
}
const spotList = $('#spotList');
const scrollSpotIntoView = (id) => spotList.querySelector(`[data-id="${id}"]`)?.scrollIntoView({ block: 'nearest', inline: 'center', behavior: 'smooth' });
HOTSPOTS.forEach((h, i) => {
  const b = el('button', { class: 'seg', type: 'button', 'data-id': h.id }, `${i + 1} · ${h.title}`);
  b.addEventListener('click', () => { if (car) car.select(h.id); else showSpot(h.id); });
  spotList.appendChild(b);
});
panel.innerHTML = '<h3>Mapa de inspección</h3><p class="muted" style="margin:6px 0 0">Tocá un número en el auto o elegí un punto abajo: te dice qué buscar y te lleva a su checklist.</p>';

let car = null;
const stage = $('#stage');
import('./car3d.js').then(({ mountCar }) => {
  car = mountCar(stage, {
    hotspots: HOTSPOTS,
    onSelect: (id) => { showSpot(id); $('#stageHint').style.opacity = 0; },
  });
  if (!car) stage.classList.add('no-webgl');
}).catch(() => stage.classList.add('no-webgl'));
document.querySelectorAll('[data-view]').forEach((b) => b.addEventListener('click', () => car && car.setView(b.dataset.view)));

// ---------------------------------------------------------------- MOT chart
const MOT = [
  { d: '2019-03-19', mi: 29209, adv: [] },
  { d: '2020-01-27', mi: 41450, adv: [] },
  { d: '2021-01-12', mi: 48404, adv: [] },
  { d: '2022-01-07', mi: 57830, adv: ['Disco trasero derecho gastado (no excesivo)', 'Pastillas delanteras y traseras finas', 'Sillas de bebé tapando cinturones'] },
  { d: '2022-12-31', mi: 66759, adv: ['Cubierta trasera derecha con grietas leves', 'Sillas de bebé tapando cinturones'] },
  { d: '2024-01-12', mi: 76615, adv: ['Freno trasero derecho fluctuante', 'Cubierta trasera derecha cerca del límite (bordes)', 'Cubierta trasera izquierda con grietas leves', 'Silla de bebé tapando cinturón'] },
  { d: '2025-01-02', mi: 83047, adv: [] },
  { d: '2026-01-14', mi: 90300, adv: [] },
];
const LISTING = { d: '2026-10-27', mi: 95291 };
const fmt = (n) => n.toLocaleString('es-AR');
const fdate = (s) => new Date(s + 'T12:00:00').toLocaleDateString('es-AR', { day: 'numeric', month: 'short', year: 'numeric' });

function drawChart() {
  const box = $('#motChart');
  const W = Math.max(280, box.clientWidth), H = 220;
  const m = { l: 44, r: 12, t: 22, b: 26 };
  const t0 = Date.parse('2019-01-01'), t1 = Date.parse('2026-12-31');
  const x = (d) => m.l + (Date.parse(d) - t0) / (t1 - t0) * (W - m.l - m.r);
  const y = (v) => H - m.b - v / 100000 * (H - m.t - m.b);
  const gy = [0, 25000, 50000, 75000, 100000];
  const years = [2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026];
  const pts = MOT.map((p) => [x(p.d), y(p.mi)]);
  const path = pts.map((p, i) => `${i ? 'L' : 'M'}${p[0].toFixed(1)} ${p[1].toFixed(1)}`).join(' ');
  const last = pts[pts.length - 1];
  const lx = x(LISTING.d), ly = y(LISTING.mi);
  const svg = `<svg viewBox="0 0 ${W} ${H}" width="${W}" height="${H}" role="img" aria-label="Millaje en cada MOT, de 29.209 millas en 2019 a 90.300 en 2026; el aviso marca 95.291 en octubre de 2026.">
    <g class="grid">${gy.map((v) => `<line x1="${m.l}" x2="${W - m.r}" y1="${y(v)}" y2="${y(v)}"/>`).join('')}</g>
    <g class="axis">${gy.map((v) => `<text x="${m.l - 8}" y="${y(v) + 4}" text-anchor="end">${v ? v / 1000 + 'k' : '0'}</text>`).join('')}
      ${years.filter((yr, i) => W > 420 || i % 2 === 0).map((yr) => `<text x="${x(yr + '-07-01')}" y="${H - 6}" text-anchor="middle">${W > 420 ? yr : "'" + String(yr).slice(2)}</text>`).join('')}</g>
    <line class="cross" id="cross" x1="0" x2="0" y1="${m.t}" y2="${H - m.b}" style="display:none"/>
    <path class="series" d="${path}"/>
    <path class="proj" d="M${last[0]} ${last[1]} L${lx} ${ly}"/>
    ${MOT.map((p, i) => `<circle class="dot${p.adv.length ? ' adv' : ''}" cx="${pts[i][0]}" cy="${pts[i][1]}" r="${p.adv.length ? 5 : 4.5}"/>`).join('')}
    <circle class="dot adv" cx="${lx}" cy="${ly}" r="4" style="stroke-dasharray:2 2"/>
    <text class="lbl" x="${pts[0][0] + 10}" y="${pts[0][1] + 16}" text-anchor="start">${fmt(MOT[0].mi)}</text>
    <text class="lbl" x="${lx}" y="${ly - 10}" text-anchor="end">${fmt(LISTING.mi)}</text>
  </svg>`;
  box.querySelector('svg')?.remove();
  box.insertAdjacentHTML('afterbegin', svg);

  const tip = $('#motTip');
  const all = [...MOT.map((p, i) => ({ ...p, px: pts[i] })), { ...LISTING, adv: null, px: [lx, ly], listing: true }];
  const s = box.querySelector('svg');
  const cross = s.querySelector('#cross');
  function show(clientX) {
    const r = s.getBoundingClientRect();
    const px = (clientX - r.left) * (W / r.width);
    let best = all[0];
    for (const p of all) if (Math.abs(p.px[0] - px) < Math.abs(best.px[0] - px)) best = p;
    cross.setAttribute('x1', best.px[0]); cross.setAttribute('x2', best.px[0]); cross.style.display = '';
    const prev = all[all.indexOf(best) - 1];
    const delta = prev ? `+${fmt(best.mi - prev.mi)} desde el anterior` : 'primer MOT';
    tip.innerHTML = best.listing
      ? `<b>${fmt(best.mi)} mi</b> · aviso (oct 2026)<br>${delta}`
      : `<b>${fmt(best.mi)} mi</b> · ${fdate(best.d)}<br>MOT aprobado · ${delta}${best.adv.length ? '<br>• ' + best.adv.join('<br>• ') : '<br>Sin advisories'}`;
    const sx = best.px[0] / W * r.width, sy = best.px[1] / H * r.height;
    tip.style.left = `${Math.min(Math.max(sx, 118), r.width - 118)}px`;
    tip.style.top = `${sy}px`;
    tip.classList.add('on');
  }
  const hide = () => { tip.classList.remove('on'); cross.style.display = 'none'; };
  s.addEventListener('pointermove', (e) => show(e.clientX));
  s.addEventListener('pointerdown', (e) => show(e.clientX));
  s.addEventListener('pointerleave', (e) => { if (e.pointerType === 'mouse') hide(); });
  hideChartTip = hide;
}
let hideChartTip = () => {};
document.addEventListener('pointerdown', (e) => { if (!$('#motChart').contains(e.target)) hideChartTip(); });

$('#motTable').innerHTML = `<table><thead><tr><th>Fecha</th><th>Millas</th><th>Resultado</th><th>Advisories</th></tr></thead><tbody>${MOT.map((p) => `<tr><td class="num-c">${fdate(p.d)}</td><td class="num-c">${fmt(p.mi)}</td><td>Aprobado</td><td>${p.adv.length ? p.adv.join('; ') : '—'}</td></tr>`).join('')}<tr><td class="num-c">oct 2026</td><td class="num-c">${fmt(LISTING.mi)}</td><td colspan="2">Millaje del aviso</td></tr></tbody></table>`;

// ---------------------------------------------------------------- price ladder
function drawLadder() {
  const box = $('#ladder');
  const W = Math.max(280, box.clientWidth), H = 150;
  const lo = 6450, hi = 7100, m = { l: 8, r: 8 };
  const x = (v) => m.l + (v - lo) / (hi - lo) * (W - m.l - m.r);
  const trackY = 78;
  const marks = [
    { v: 6700, t: 'Primera oferta', up: true },
    { v: 6800, t: 'Objetivo', up: false },
    { v: 6900, t: 'Techo sin extras', up: true },
    { v: 6995, t: 'Publicado', up: false, list: true },
  ];
  const small = W < 420;
  box.innerHTML = `<svg viewBox="0 0 ${W} ${H}" width="${W}" height="${H}" role="img" aria-label="Escala de precios: rango Good de Autotrader de £6.652 a £6.992; primera oferta £6.700, objetivo £6.800, techo £6.900, publicado £6.995.">
    <rect x="${x(lo)}" y="${trackY - 9}" width="${x(hi) - x(lo)}" height="18" rx="9" fill="var(--surface-2)"/>
    <rect x="${x(6652)}" y="${trackY - 9}" width="${x(6992) - x(6652)}" height="18" rx="4" fill="var(--accent-soft)" stroke="var(--accent)" stroke-width="1.5"/>
    ${marks.map((k) => {
      const X = x(k.v), y1 = k.up ? trackY - 12 : trackY + 12, y2 = k.up ? 34 : H - 22;
      const anchor = X > W - 70 ? 'end' : X < 70 ? 'start' : 'middle';
      const col = k.list ? 'var(--text)' : 'var(--accent)';
      return `<line x1="${X}" x2="${X}" y1="${y1}" y2="${y2 + (k.up ? 6 : -6)}" stroke="${col}" stroke-width="2"/>
        <circle cx="${X}" cy="${trackY}" r="${k.list ? 6 : 5}" fill="${k.list ? 'var(--surface)' : col}" stroke="${col}" stroke-width="2.5"/>
        <text x="${X}" y="${k.up ? y2 - 12 : y2 + 4}" text-anchor="${anchor}" font-size="12" font-weight="700" fill="var(--text)">£${fmt(k.v)}</text>
        <text x="${X}" y="${k.up ? y2 + 1 : y2 + 17}" text-anchor="${anchor}" font-size="11" fill="var(--text-2)">${k.t}</text>`;
    }).join('')}
  </svg>
  <div class="legend"><span><i class="sw" style="border-radius:3px;background:var(--accent-soft);box-shadow:inset 0 0 0 1.5px var(--accent)"></i>Rango "Good" de Autotrader: £6.652–6.992</span><span>izquierda = más barato · derecha = "Fair"</span></div>`;
}

// ---------------------------------------------------------------- phrases
const PHRASES = [
  ['We like the car. What’s the best price you can do if we pay in full today?', 'Nos gusta. ¿Cuál es el mejor precio pagando todo hoy?'],
  ['Autotrader has it right at the top of its “good price” range, and it’s about to go over 100,000 miles.', 'Autotrader lo pone en el techo del rango "good" y está por pasar las 100.000 millas.'],
  ['The MOT history shows a rear brake advisory and several tyre advisories, so we’d need to budget for that.', 'El MOT marca el freno trasero y las cubiertas: vamos a tener que gastar en eso.'],
  ['We can do £6,700 today, paid in full.', 'Podemos pagar £6.700 hoy, en un pago.'],
  ['If you can meet us at £6,800, we’ll do it now.', 'Si llegás a £6.800, lo cerramos ahora.'],
  ['If the price can’t move, could you include two new tyres, the CVT fluid change or a 12-month warranty?', 'Si no baja, ¿incluyen 2 cubiertas, el cambio de aceite de la CVT o 12 meses de garantía?'],
  ['Can you put everything that’s included on the invoice — the MOT, the warranty terms and any work done?', '¿Pueden poner todo lo incluido en la factura: MOT, garantía y trabajos?'],
  ['Thanks — we’ve got another one to see today. Let us know if anything changes.', 'Gracias, tenemos otro para ver hoy. Avisennos si cambia algo.'],
];
$('#phrases').innerHTML = PHRASES.map(([en, es], i) => `<div class="say"><button class="copy" type="button" data-i="${i}">copiar</button><div class="en">“${en}”</div><div class="es">${es}</div></div>`).join('');
$('#phrases').addEventListener('click', async (e) => {
  const b = e.target.closest('.copy'); if (!b) return;
  try { await navigator.clipboard.writeText(PHRASES[b.dataset.i][0]); toast('Copiado'); } catch { toast('No se pudo copiar'); }
});

// ---------------------------------------------------------------- nav highlighting
const chips = [...document.querySelectorAll('#chips .chip')];
const secs = chips.map((c) => document.querySelector(c.getAttribute('href')));
const io = new IntersectionObserver((entries) => {
  for (const en of entries) if (en.isIntersecting) {
    const i = secs.indexOf(en.target);
    chips.forEach((c, j) => c.classList.toggle('is-on', j === i));
    chips[i]?.scrollIntoView({ block: 'nearest', inline: 'center', behavior: 'smooth' });
  }
}, { rootMargin: '-45% 0px -50% 0px' });
secs.forEach((s) => s && io.observe(s));

// ---------------------------------------------------------------- boot
renderChecklists();
drawChart();
drawLadder();
let rt, lastW = $('main').clientWidth;
new ResizeObserver(() => {
  const w = $('main').clientWidth;
  if (w === lastW) return;
  lastW = w; clearTimeout(rt); rt = setTimeout(() => { drawChart(); drawLadder(); }, 80);
}).observe($('main'));
if ('serviceWorker' in navigator && location.protocol === 'https:') navigator.serviceWorker.register('./sw.js').catch(() => {});

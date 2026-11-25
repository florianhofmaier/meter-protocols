import type { AnalysisResponse, Field, Report } from './contracts';
import { fieldSpan } from './contracts';

export function download(name: string, content: string, type: string) {
  const url = URL.createObjectURL(new Blob([content], { type }));
  const link = document.createElement('a');
  link.href = url;
  link.download = name;
  link.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}
export function saveJson(response: AnalysisResponse) {
  download('protocol-analysis.json', JSON.stringify(response, null, 2), 'application/json');
}
const escape = (text: string) =>
  text.replace(
    /[&<>"']/g,
    (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]!,
  );

/** Build a self-contained, printable document from the same neutral report shown by the UI. */
export function documentHtml(report: Report): string {
  const renderField = (field: Field, depth: number): string => {
    const span = fieldSpan(field);
    const source = report.sources.find((s) => s.id === span.sourceId);
    const raw =
      source?.hex
        .slice(span.offset * 2, (span.offset + span.length) * 2)
        .match(/../g)
        ?.join(' ') ?? '';
    return `<section class="field"><h${Math.min(3 + depth, 6)}>${escape(field.name)}</h${Math.min(3 + depth, 6)}><p>Feld ${escape(field.id)} · Source ${escape(span.sourceId)} · Offset ${span.offset} · ${span.length} Bytes · ${field.observation.kind === 'parsed' ? 'gelesen' : 'unvollständig'}</p><code>${escape(raw)}</code><p>${escape(field.documentation.description)}</p>${field.documentation.reference ? `<p class="reference">${escape(field.documentation.reference)}</p>` : ''}${field.observation.kind === 'incomplete' ? `<p>${escape(field.observation.reason)}</p>` : ''}${field.values.length ? `<dl>${field.values.map((v) => `<dt>${escape(v.name)}</dt><dd>${escape(v.text)}</dd>`).join('')}</dl>` : ''}${field.children.map((c) => renderField(c, depth + 1)).join('')}</section>`;
  };
  return `<!doctype html><html lang="de"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Protocol Inspector – Analysebericht</title><style>body{font:15px/1.6 system-ui,sans-serif;max-width:1000px;margin:40px auto;padding:0 24px;color:#172333}h1,h2,h3{line-height:1.3}code,pre{font-family:ui-monospace,monospace;overflow-wrap:anywhere;white-space:pre-wrap}.field{border-left:2px solid #dae2eb;padding-left:16px;margin:20px 0}.reference{color:#526277}dt{font-weight:600}dd{margin-left:0;overflow-wrap:anywhere}@media print{body{margin:0;max-width:none}.field{break-inside:auto}h2,h3,h4,h5,h6{break-after:avoid}}</style><h1>Analysebericht · ${escape(report.protocol.name)}</h1>${report.message ? `<h2>${escape(report.message.name)}</h2><p>${escape(report.message.direction)} · ${escape(report.message.description)}</p><p class="reference">${escape(report.message.reference)} · Feld ${escape(report.message.fieldId)}</p>` : ''}<p>${escape(report.protocol.scope)}</p><p>Schema-Version ${report.schemaVersion} · ${report.allStepsSucceeded ? 'Alle Analyseschritte erfolgreich' : 'Analyse mit fehlgeschlagenen oder blockierten Schritten'}. Dies ist keine vollständige Normkonformitätsaussage.</p><h2>Sources</h2>${report.sources.map((s) => `<h3>${escape(s.name)} · ${s.hex.length / 2} Bytes</h3><p>Source ${escape(s.id)}${s.sensitive ? ' · sensible abgeleitete Daten' : ''}</p><pre>${escape(s.hex.match(/../g)?.join(' ') ?? '')}</pre>`).join('')}<h2>Diagnosen</h2>${report.diagnostics.length ? `<ul>${report.diagnostics.map((d) => `<li><strong>${escape(d.severity)}</strong> · ${escape(d.message)}</li>`).join('')}</ul>` : '<p>Keine Diagnosen gemeldet.</p>'}<h2>Feldstruktur</h2>${report.fields.map((f) => renderField(f, 0)).join('')}<h2>Analyseablauf</h2><ol>${report.steps.map((s) => `<li>${escape(s.name)} · ${escape(s.outcome.kind)}${s.outcome.kind === 'blocked' ? ` · Ursache: Schritt ${escape(s.outcome.causeId)} · ${escape(s.outcome.reason)}` : ''}</li>`).join('')}</ol></html>`;
}

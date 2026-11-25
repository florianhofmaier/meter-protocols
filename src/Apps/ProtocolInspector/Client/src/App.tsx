import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import type { AnalysisResponse, Diagnostic, Field, Report, Source, Span } from './contracts';
import { fieldAtByte, fieldSpan, flattenFields, readResponse } from './contracts';
import { documentHtml, download, saveJson } from './report';

const example = '68 15 15 68 08 01 72 78 56 34 12 2D 2C 01 07 08 00 00 00 04 13 39 30 00 00 78 16';
const PAGE_SIZE = 256;
const formatHex = (hex: string) => hex.match(/../g)?.join(' ') ?? '';
const offsetText = (offset: number) => offset.toString(16).toUpperCase().padStart(4, '0');
const confidenceText = (confidence: string) =>
  ({
    high: 'Starke Übereinstimmung',
    medium: 'Plausibler Kandidat',
    low: 'Schwache Übereinstimmung',
  })[confidence] ?? confidence;

export default function App() {
  const [input, setInput] = useState('');
  const [response, setResponse] = useState<AnalysisResponse | null>(null);
  const [imported, setImported] = useState(false);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState('');
  const [selectedInputId, setSelectedInputId] = useState('');
  const [selectedCandidateId, setSelectedCandidateId] = useState('');
  const [sourceId, setSourceId] = useState('');
  const [selectedId, setSelectedId] = useState<string | null>(null);
  const [cursor, setCursor] = useState<Span | null>(null);
  const [page, setPage] = useState(0);
  const [query, setQuery] = useState('');
  const [tab, setTab] = useState<'structure' | 'diagnostics' | 'steps'>('structure');
  const [showEvidence, setShowEvidence] = useState(false);
  const [notice, setNotice] = useState('');
  const file = useRef<HTMLInputElement>(null);
  const pending = useRef<AbortController | null>(null);

  const acceptResponse = useCallback((result: AnalysisResponse) => {
    setResponse(result);
    const chosen =
      result.candidates.find((c) => c.id === result.selectedCandidateId) ?? result.candidates[0];
    setSelectedCandidateId(chosen?.id ?? '');
    setSelectedInputId(chosen?.inputId ?? result.inputs[0]?.id ?? '');
    setSourceId('');
    setSelectedId(null);
    setCursor(null);
    setPage(0);
    setQuery('');
  }, []);

  const analyze = useCallback(
    async (text: string) => {
      pending.current?.abort();
      if (!text.trim()) {
        setResponse(null);
        setBusy(false);
        return;
      }
      const controller = new AbortController();
      pending.current = controller;
      setBusy(true);
      setError('');
      try {
        const result = await fetch('/api/analyze', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ input: text }),
          signal: controller.signal,
        });
        if (!result.ok) {
          const body = await result.json().catch(() => null);
          throw new Error(
            body?.message ?? `Der Analysedienst antwortet mit HTTP ${result.status}.`,
          );
        }
        const report = readResponse(await result.json());
        if (!controller.signal.aborted) acceptResponse(report);
      } catch (cause) {
        if (!controller.signal.aborted) {
          setError(cause instanceof Error ? cause.message : 'Die Analyse ist fehlgeschlagen.');
          setResponse(null);
        }
      } finally {
        if (!controller.signal.aborted) setBusy(false);
      }
    },
    [acceptResponse],
  );

  useEffect(() => {
    if (imported) return;
    const timer = setTimeout(() => void analyze(input), 500);
    return () => {
      clearTimeout(timer);
      pending.current?.abort();
    };
  }, [input, imported, analyze]);

  const candidate =
    response?.candidates.find(
      (c) => c.id === selectedCandidateId && c.inputId === selectedInputId,
    ) ?? response?.candidates.find((c) => c.inputId === selectedInputId);
  const report = candidate?.report;
  const selectedInput = response?.inputs.find((i) => i.id === selectedInputId);
  const sources: Source[] =
    report?.sources ??
    (selectedInput
      ? [
          {
            id: 'input',
            name: 'Interpretierte Eingabe',
            hex: selectedInput.hex,
            sensitive: false,
            origin: null,
          },
        ]
      : []);
  const source = sources.find((s) => s.id === sourceId) ?? sources[0];
  const allFields = useMemo(() => flattenFields(report?.fields ?? []), [report]);
  const selected = allFields.find((f) => f.id === selectedId);
  const selection = selected ? fieldSpan(selected) : cursor;
  const diagnostics = report?.diagnostics ?? [];
  const errors = diagnostics.filter((d) => d.severity === 'error').length;
  const warnings = diagnostics.filter((d) => d.severity === 'warning').length;

  function changeInput(text: string) {
    pending.current?.abort();
    setImported(false);
    setInput(text);
    setResponse(null);
    setError('');
    setNotice('');
    setBusy(Boolean(text.trim()));
  }
  function selectField(field: Field) {
    const span = fieldSpan(field);
    setSelectedId(field.id);
    setCursor(null);
    setSourceId(span.sourceId);
    setPage(Math.floor(span.offset / PAGE_SIZE));
  }
  function selectDiagnostic(diagnostic: Diagnostic) {
    const field = allFields.find((f) => f.id === diagnostic.fieldId);
    if (field) selectField(field);
    else if (diagnostic.span) {
      setSelectedId(null);
      setCursor(diagnostic.span);
      setSourceId(diagnostic.span.sourceId);
      setPage(Math.floor(diagnostic.span.offset / PAGE_SIZE));
    }
    setTab('structure');
  }
  function chooseInput(id: string) {
    setSelectedInputId(id);
    setSelectedCandidateId(response?.candidates.find((c) => c.inputId === id)?.id ?? '');
    setSelectedId(null);
    setCursor(null);
    setSourceId('');
    setPage(0);
  }
  async function openFile(selectedFile: File) {
    pending.current?.abort();
    setBusy(false);
    setError('');
    setNotice('');
    if (selectedFile.size > 2 * 1024 * 1024) {
      setError('Die Datei ist zu groß (maximal 2 MiB).');
      return;
    }
    try {
      if (/\.(bin|dat)$/i.test(selectedFile.name)) {
        const bytes = new Uint8Array(await selectedFile.arrayBuffer());
        if (bytes.length > 65536)
          throw new Error('Binäre Eingaben dürfen höchstens 65 536 Bytes enthalten.');
        changeInput(Array.from(bytes, (b) => b.toString(16).padStart(2, '0')).join(' '));
      } else {
        const text = await selectedFile.text();
        if (text.trimStart().startsWith('{')) {
          const loaded = readResponse(JSON.parse(text));
          setImported(true);
          setInput('');
          acceptResponse(loaded);
          setNotice(`Bericht geöffnet: ${selectedFile.name}`);
        } else changeInput(text);
      }
    } catch (cause) {
      setError(cause instanceof Error ? cause.message : 'Datei konnte nicht geöffnet werden.');
    }
  }
  function exportWithSensitiveCheck(action: () => void) {
    if (
      !response?.candidates.some((c) => c.report.sources.some((s) => s.sensitive)) ||
      window.confirm(
        'Dieser Bericht enthält sensible abgeleitete Daten. Diese Daten mit exportieren?',
      )
    )
      action();
  }

  return (
    <div className="app-shell">
      <header className="topbar">
        <a className="brand" href="/" aria-label="Protocol Inspector Startseite">
          <span className="brand-mark" aria-hidden="true">
            ⌘
          </span>
          <span>
            Protocol<span className="brand-light">Inspector</span>
          </span>
        </a>
        <div className="topbar-right">
          <span className="availability">
            <i /> M-Bus verfügbar
          </span>
          <span className="future-protocol">DLMS / COSEM · geplant</span>
        </div>
      </header>
      <main>
        <div className="page-heading">
          <div>
            <div className="eyebrow">TELEGRAMM-ANALYSE</div>
            <h1>Von Bytes zu Bedeutung.</h1>
            <p>Telegramm einfügen. Interpretation erkennen. Struktur verstehen.</p>
          </div>
          <span className="version-label">
            LOCAL WORKSPACE <span>v0.1</span>
          </span>
        </div>
        <section className="input-panel" aria-labelledby="input-title">
          <div className="section-toolbar">
            <h2 id="input-title">
              <span className="section-number">01</span> Eingabe
            </h2>
            <div className="actions">
              <button type="button" onClick={() => file.current?.click()}>
                Datei öffnen
              </button>
              <input
                ref={file}
                type="file"
                className="visually-hidden"
                aria-label="Telegramm oder Analysebericht öffnen"
                accept=".txt,.hex,.json,.bin,.dat"
                onChange={(event) => {
                  const selectedFile = event.target.files?.[0];
                  if (selectedFile) void openFile(selectedFile);
                  event.target.value = '';
                }}
              />
            </div>
          </div>
          <form
            onSubmit={(event) => {
              event.preventDefault();
              if (!imported) void analyze(input);
            }}
          >
            <label className="visually-hidden" htmlFor="telegram">
              Telegramm als Hex oder Bytearray
            </label>
            <textarea
              id="telegram"
              spellCheck={false}
              value={input}
              onChange={(event) => changeInput(event.target.value)}
              placeholder={
                imported
                  ? 'Gespeicherter Bericht geöffnet. Hier ein neues Telegramm eingeben …'
                  : '68 15 15 68 08 01 72 …\n\nHex, 0x-Bytes, \\x-Bytes, JSON-Bytearray'
              }
            />
            <div className="input-bottom">
              <div className="examples">
                <span>Beispiele</span>
                <button type="button" onClick={() => changeInput(example)}>
                  Messwert
                </button>
                <button
                  type="button"
                  onClick={() => changeInput(example.replace('78 16', '79 16'))}
                >
                  Prüfsummenfehler
                </button>
                <button type="button" onClick={() => changeInput('E5')}>
                  ACK
                </button>
              </div>
              <button className="primary-button" type="submit" disabled={!input.trim() || imported}>
                {busy ? 'Analysiert …' : 'Analysieren'} <span aria-hidden="true">↗</span>
              </button>
            </div>
          </form>
          <div className="input-hint">
            Automatische Analyse nach der Eingabe · Aktuell: ein einzelnes wired M-Bus-Telegramm
          </div>
        </section>
        {error && (
          <div className="error-banner" role="alert">
            <strong>Analyse nicht verfügbar</strong>
            <span>{error}</span>
          </div>
        )}
        {notice && (
          <div className="notice" role="status">
            {notice}
          </div>
        )}
        <div aria-live="polite" className="visually-hidden">
          {busy
            ? 'Analyse läuft.'
            : response
              ? `${response.candidates.length} mögliche Interpretation(en). ${errors} Fehler.`
              : ''}
        </div>

        {response && (
          <>
            <section className="interpretation-bar" aria-label="Erkannte Interpretation">
              <div className="interpretation-main">
                <span
                  className={`signal ${report ? candidate?.confidence : 'unknown'}`}
                  aria-hidden="true"
                >
                  {report ? '◈' : '?'}
                </span>
                <div>
                  <div className="eyebrow">
                    {imported ? 'GESPEICHERTE ANALYSE' : 'PLAUSIBELSTE INTERPRETATION'}
                  </div>
                  <h2>
                    {report ? report.protocol.name : 'Protokoll nicht erkannt'}
                    {report?.message && ` · ${report.message.name}`}{' '}
                    <span>
                      {report ? '· ' + report.protocol.scope : '· Bytes bleiben verfügbar'}
                    </span>
                  </h2>
                  {report?.message && (
                    <p className="message-summary">
                      {report.message.direction} · {report.message.description}{' '}
                      <button
                        type="button"
                        title={report.message.reference}
                        onClick={() => {
                          const field = allFields.find((f) => f.id === report.message?.fieldId);
                          if (field) selectField(field);
                        }}
                      >
                        Typbestimmendes Feld ↗
                      </button>
                    </p>
                  )}
                </div>
              </div>
              <div className="interpretation-meta">
                <span>{source ? `${source.hex.length / 2} Bytes` : 'Keine Bytes'}</span>
                {report && (
                  <span className={`confidence ${candidate?.confidence}`}>
                    {confidenceText(candidate?.confidence ?? '')}
                  </span>
                )}
                <button
                  type="button"
                  onClick={() => setShowEvidence(!showEvidence)}
                  aria-expanded={showEvidence}
                >
                  Erkennung {showEvidence ? '−' : '+'}
                </button>
              </div>
            </section>
            {showEvidence && (
              <section className="evidence-panel">
                <h3>Warum diese Interpretation?</h3>
                <ul>
                  {candidate?.evidence.map((item, i) => (
                    <li key={i}>{item}</li>
                  ))}
                  {response.messages.map((message, i) => (
                    <li key={`message-${i}`}>{message}</li>
                  ))}
                </ul>
                {response.inputs.length > 1 && (
                  <label>
                    Eingabedarstellung
                    <select value={selectedInputId} onChange={(e) => chooseInput(e.target.value)}>
                      {response.inputs.map((i) => (
                        <option key={i.id} value={i.id}>
                          {i.encoding} · {i.hex.length / 2} Bytes
                        </option>
                      ))}
                    </select>
                  </label>
                )}
                {response.candidates.filter((c) => c.inputId === selectedInputId).length > 1 && (
                  <label>
                    Protokollinterpretation
                    <select
                      value={candidate?.id}
                      onChange={(e) => {
                        setSelectedCandidateId(e.target.value);
                        setSelectedId(null);
                        setSourceId('');
                        setPage(0);
                      }}
                    >
                      {response.candidates
                        .filter((c) => c.inputId === selectedInputId)
                        .map((c) => (
                          <option key={c.id} value={c.id}>
                            {c.report.protocol.name} · {confidenceText(c.confidence)}
                          </option>
                        ))}
                    </select>
                  </label>
                )}
              </section>
            )}
            {!report && <div className="unknown-message">{response.messages[0]}</div>}
            <section className="analysis-panel" aria-label="Telegrammstruktur">
              <div className="analysis-toolbar">
                <div className="tabs" role="tablist" aria-label="Analyseansichten">
                  {(['structure', 'diagnostics', 'steps'] as const).map((t) => (
                    <button
                      key={t}
                      type="button"
                      role="tab"
                      id={`tab-${t}`}
                      aria-controls={`panel-${t}`}
                      aria-selected={tab === t}
                      onClick={() => setTab(t)}
                    >
                      {t === 'structure'
                        ? 'Struktur'
                        : t === 'diagnostics'
                          ? 'Diagnosen'
                          : 'Analyseablauf'}
                      {t === 'diagnostics' && diagnostics.length > 0 && (
                        <span className={errors ? 'error-count' : ''}>{diagnostics.length}</span>
                      )}
                    </button>
                  ))}
                </div>
                <div className="export-actions">
                  <button
                    type="button"
                    disabled={!report}
                    onClick={() => exportWithSensitiveCheck(() => saveJson(response))}
                  >
                    JSON ↓
                  </button>
                  <button
                    type="button"
                    disabled={!report}
                    onClick={() =>
                      report &&
                      exportWithSensitiveCheck(() =>
                        download('protocol-analysis.html', documentHtml(report), 'text/html'),
                      )
                    }
                  >
                    Bericht HTML ↓
                  </button>
                </div>
              </div>
              <div id={`panel-${tab}`} role="tabpanel" aria-labelledby={`tab-${tab}`}>
                {tab === 'structure' && (
                  <div className="inspector-grid">
                    <aside className="tree-pane">
                      <div className="pane-heading">
                        <h3>Feldstruktur</h3>
                        <span>{allFields.length} Felder</span>
                      </div>
                      <label className="visually-hidden" htmlFor="field-search">
                        Feld suchen
                      </label>
                      <input
                        id="field-search"
                        className="field-search"
                        placeholder="Feld suchen …"
                        value={query}
                        onChange={(event) => setQuery(event.target.value)}
                      />
                      <div className="tree-content">
                        {report ? (
                          <FieldTree
                            fields={report.fields}
                            selectedId={selectedId}
                            diagnostics={diagnostics}
                            query={query}
                            onSelect={selectField}
                          />
                        ) : (
                          <p className="muted">
                            Für diese Eingabe ist kein Strukturparser verfügbar.
                          </p>
                        )}
                      </div>
                    </aside>
                    <section className="hex-pane">
                      <div className="pane-heading">
                        <h3>Bytes</h3>
                        <span>HEX</span>
                      </div>
                      {sources.length > 1 ? (
                        <label className="source-select">
                          Source
                          <select
                            value={source?.id}
                            onChange={(e) => {
                              setSourceId(e.target.value);
                              setSelectedId(null);
                              setCursor(null);
                              setPage(0);
                            }}
                          >
                            {sources.map((s) => (
                              <option key={s.id} value={s.id}>
                                {s.name}
                                {s.sensitive ? ' · sensibel' : ''}
                              </option>
                            ))}
                          </select>
                        </label>
                      ) : (
                        <div className="source-name">
                          {source?.name ?? 'Keine Source'} <span>{selectedInput?.encoding}</span>
                        </div>
                      )}
                      {source && (
                        <HexView
                          source={source}
                          selection={selection}
                          page={page}
                          onPage={setPage}
                          onByte={(offset) => {
                            const field = fieldAtByte(report?.fields ?? [], source.id, offset);
                            if (field) selectField(field);
                            else {
                              setSelectedId(null);
                              setCursor({ sourceId: source.id, offset, length: 1 });
                            }
                          }}
                        />
                      )}
                      {selection && (
                        <div className="selection-caption">
                          {selection.length
                            ? `${selection.length} Byte${selection.length === 1 ? '' : 's'} ausgewählt`
                            : 'Bekannter Start / Fehlercursor'}{' '}
                          · Offset {selection.offset} / 0x{offsetText(selection.offset)}
                        </div>
                      )}
                      <div className="hex-legend">
                        <span>
                          <i className="legend-selection" /> Auswahl
                        </span>
                        <span>Offsets relativ zur Source</span>
                      </div>
                    </section>
                    <aside className="detail-pane">
                      <div className="pane-heading">
                        <h3>Felddetails</h3>
                        {selected && <span>#{selected.id}</span>}
                      </div>
                      {selected && report ? (
                        <FieldDetails field={selected} report={report} />
                      ) : cursor && source ? (
                        <>
                          <h4>Byteposition</h4>
                          <div className="mono">
                            Offset {cursor.offset} · 0x{offsetText(cursor.offset)}
                          </div>
                          <p className="muted">
                            {cursor.offset >= source.hex.length / 2
                              ? 'Position am Ende der Source; es wurde kein weiteres Byte gelesen.'
                              : 'Für diese Position wurde kein vollständiges Feld abgegrenzt.'}
                          </p>
                        </>
                      ) : (
                        <div className="detail-empty">
                          <span aria-hidden="true">⌖</span>
                          <h4>Ein Feld, alle Details.</h4>
                          <p>
                            Ein Feld oder Byte auswählen, um Rohwert, Bedeutung, geprüfte Werte und
                            Normstelle zu sehen.
                          </p>
                        </div>
                      )}
                    </aside>
                  </div>
                )}
                {tab === 'diagnostics' && (
                  <div className="diagnostics-view">
                    <div className="view-heading">
                      <h3>Diagnosen</h3>
                      <span>
                        {errors} Fehler · {warnings} Warnungen
                      </span>
                    </div>
                    {diagnostics.length === 0 ? (
                      <div className="empty-state small">
                        <h3>Keine Diagnosen gemeldet.</h3>
                        <p>Dies ist keine Aussage zur vollständigen Normkonformität.</p>
                      </div>
                    ) : (
                      diagnostics.map((d) => (
                        <button
                          key={d.id}
                          type="button"
                          className="diagnostic-row"
                          onClick={() => selectDiagnostic(d)}
                        >
                          <span className={`diagnostic-icon ${d.severity}`}>
                            {d.severity === 'error' ? '!' : 'i'}
                          </span>
                          <span>
                            <strong>
                              {d.kind === 'parse'
                                ? 'Struktur'
                                : d.kind === 'validation'
                                  ? 'Validierung'
                                  : d.kind === 'protection'
                                    ? 'Schutzverarbeitung'
                                    : 'Hinweis'}
                            </strong>
                            <span>{d.message}</span>
                            <small>
                              {d.fieldId !== null ? `Feld #${d.fieldId}` : 'Ohne Feldzuordnung'}
                              {d.span
                                ? ` · Source ${d.span.sourceId} · Offset ${d.span.offset}`
                                : ''}
                            </small>
                          </span>
                          <span aria-hidden="true">↗</span>
                        </button>
                      ))
                    )}
                  </div>
                )}
                {tab === 'steps' && (
                  <div className="steps-view">
                    <div className="view-heading">
                      <h3>Analyseablauf</h3>
                      <span>Lesen · Validieren · Schutzverarbeitung</span>
                    </div>
                    {report?.steps.map((step) => (
                      <div className="step-row" key={step.id}>
                        <span className="step-id">{step.id.padStart(2, '0')}</span>
                        <span className={`step-symbol ${step.outcome.kind}`}>
                          {step.outcome.kind === 'succeeded'
                            ? '✓'
                            : step.outcome.kind === 'failed'
                              ? '!'
                              : '—'}
                        </span>
                        <div>
                          <strong>{step.name}</strong>
                          <p>
                            Source {step.input.sourceId} · Offset {step.input.offset} ·{' '}
                            {step.input.length} Bytes
                            {step.dependencies.length
                              ? ` · nach Schritt ${step.dependencies.join(', ')}`
                              : ''}
                          </p>
                          {step.outcome.kind === 'blocked' && (
                            <p>
                              Blockiert durch Schritt {step.outcome.causeId}: {step.outcome.reason}
                            </p>
                          )}
                          {step.outcome.kind === 'failed' && (
                            <p>
                              {diagnostics
                                .filter(
                                  (d) =>
                                    step.outcome.kind === 'failed' &&
                                    step.outcome.diagnosticIds.includes(d.id),
                                )
                                .map((d) => d.message)
                                .join(' · ')}
                            </p>
                          )}
                        </div>
                        <span className={`step-state ${step.outcome.kind}`}>
                          {step.outcome.kind === 'succeeded'
                            ? 'Erfolgreich'
                            : step.outcome.kind === 'failed'
                              ? 'Fehlgeschlagen'
                              : 'Blockiert'}
                        </span>
                      </div>
                    )) ?? <p>Kein Protokollablauf verfügbar.</p>}
                  </div>
                )}
              </div>
              <footer className="analysis-footer">
                <span>
                  {report
                    ? report.allStepsSucceeded
                      ? '✓ Alle Analyseschritte erfolgreich'
                      : `${errors} Fehler · ${report.steps.filter((s) => s.outcome.kind === 'blocked').length} blockierte Schritte`
                    : 'Keine passende Protokollinterpretation'}
                </span>
                <span>Analyseergebnis ≠ vollständige Normkonformität</span>
              </footer>
            </section>
          </>
        )}
        {!response && !busy && !error && (
          <section className="empty-state">
            <div className="empty-glyph" aria-hidden="true">
              [ 0x ]
            </div>
            <h2>Jedes Byte hat seinen Platz.</h2>
            <p>Starte mit einem Telegramm oder öffne einen gespeicherten Bericht.</p>
            <div className="empty-features">
              <span>01 · Struktur erkennen</span>
              <span>02 · Felder untersuchen</span>
              <span>03 · Analyse dokumentieren</span>
            </div>
          </section>
        )}
        {busy && !response && (
          <div className="loading-state" role="status">
            <span className="spinner" /> Eingabe interpretieren und Telegramm analysieren …
          </div>
        )}
      </main>
      <footer className="site-footer">
        <span>Protocol Inspector</span>
        <span>Protokollneutraler Analysevertrag · M-Bus implementiert · DLMS vorbereitet</span>
      </footer>
    </div>
  );
}

function FieldTree({
  fields,
  selectedId,
  diagnostics,
  query,
  onSelect,
}: {
  fields: Field[];
  selectedId: string | null;
  diagnostics: Diagnostic[];
  query: string;
  onSelect: (field: Field) => void;
}) {
  const [collapsed, setCollapsed] = useState<Set<string>>(new Set());
  const matches = (field: Field): boolean =>
    field.name.toLowerCase().includes(query.toLowerCase()) || field.children.some(matches);
  const render = (field: Field) => {
    if (query && !matches(field)) return null;
    const problem =
      field.observation.kind === 'incomplete' ||
      diagnostics.some((d) => d.fieldId === field.id && d.severity === 'error');
    const button = (
      <button
        type="button"
        className={`tree-field ${selectedId === field.id ? 'selected' : ''}`}
        aria-pressed={selectedId === field.id}
        onClick={() => onSelect(field)}
      >
        <i className={problem ? 'field-problem' : 'field-read'} />
        <span>{field.name}</span>
        <small>{field.observation.kind === 'parsed' ? field.observation.span.length : '?'}</small>
      </button>
    );
    const expanded = Boolean(query) || !collapsed.has(field.id);
    return (
      <li key={field.id}>
        <div className="tree-line">
          {field.children.length > 0 && (
            <button
              type="button"
              className="tree-toggle"
              aria-label={`Unterfelder von ${field.name}`}
              aria-expanded={expanded}
              onClick={() =>
                setCollapsed((previous) => {
                  const next = new Set(previous);
                  if (next.has(field.id)) next.delete(field.id);
                  else next.add(field.id);
                  return next;
                })
              }
            >
              {expanded ? '⌄' : '›'}
            </button>
          )}
          {button}
        </div>
        {field.children.length > 0 && expanded && <ul>{field.children.map(render)}</ul>}
      </li>
    );
  };
  return <ul className="field-tree">{fields.map(render)}</ul>;
}

function HexView({
  source,
  selection,
  page,
  onPage,
  onByte,
}: {
  source: Source;
  selection: Span | null;
  page: number;
  onPage: (page: number) => void;
  onByte: (offset: number) => void;
}) {
  const bytes = source.hex.match(/../g) ?? [];
  const lastPage = Math.max(0, Math.ceil(bytes.length / PAGE_SIZE) - 1);
  const currentPage = Math.min(page, lastPage);
  const start = currentPage * PAGE_SIZE;
  const rows = Array.from(
    { length: Math.ceil(Math.min(PAGE_SIZE, bytes.length - start) / 16) },
    (_, i) => start + i * 16,
  );
  return (
    <>
      <div className="hex-scroll">
        <div className="hex-table">
          <div className="hex-header">
            <span>OFFSET</span>
            <span className="hex-columns">
              {Array.from({ length: 16 }, (_, i) => (
                <span key={i}>{i.toString(16).toUpperCase()}</span>
              ))}
            </span>
          </div>
          {rows.map((offset) => (
            <div className="hex-row" key={offset}>
              <span className="hex-offset">{offsetText(offset)}</span>
              <div className="hex-columns">
                {Array.from({ length: 16 }, (_, index) => {
                  const position = offset + index;
                  const byte = bytes[position];
                  const active =
                    selection?.sourceId === source.id &&
                    (selection.length
                      ? position >= selection.offset &&
                        position < selection.offset + selection.length
                      : position === selection.offset);
                  return byte ? (
                    <button
                      type="button"
                      aria-label={`Byte ${position}: ${byte}`}
                      aria-pressed={Boolean(active)}
                      className={active ? 'byte-selected' : ''}
                      key={index}
                      onClick={() => onByte(position)}
                    >
                      {byte}
                    </button>
                  ) : (
                    <span key={index} />
                  );
                })}
              </div>
            </div>
          ))}
        </div>
      </div>
      {bytes.length > PAGE_SIZE && (
        <div className="pagination">
          <button
            type="button"
            disabled={currentPage === 0}
            onClick={() => onPage(currentPage - 1)}
          >
            ← Zurück
          </button>
          <span>
            {currentPage + 1} / {lastPage + 1}
          </span>
          <button
            type="button"
            disabled={currentPage === lastPage}
            onClick={() => onPage(currentPage + 1)}
          >
            Weiter →
          </button>
        </div>
      )}
    </>
  );
}

function FieldDetails({ field, report }: { field: Field; report: Report }) {
  const span = fieldSpan(field);
  const source = report.sources.find((s) => s.id === span.sourceId);
  const raw = source?.hex.slice(span.offset * 2, (span.offset + span.length) * 2) ?? '';
  const diagnostics = report.diagnostics.filter((d) => d.fieldId === field.id);
  return (
    <div className="field-details">
      <h4>{field.name}</h4>
      <span className={`field-status ${field.observation.kind}`}>
        {field.observation.kind === 'parsed' ? 'Strukturell gelesen' : 'Unvollständig'}
      </span>
      <dl className="position-details">
        <div>
          <dt>Source</dt>
          <dd>{source?.name ?? span.sourceId}</dd>
        </div>
        <div>
          <dt>Offset</dt>
          <dd>
            {span.offset} <span>/ 0x{offsetText(span.offset)}</span>
          </dd>
        </div>
        <div>
          <dt>Länge</dt>
          <dd>
            {field.observation.kind === 'parsed' ? `${span.length} Bytes` : 'Nicht abgegrenzt'}
          </dd>
        </div>
      </dl>
      <div className="detail-block">
        <h5>ROHBYTES</h5>
        <code className="raw-value">
          {raw ? formatHex(raw) : 'Kein vollständig gelesener Bytebereich'}
        </code>
        {raw.length > 0 && raw.length <= 16 && (
          <div className="binary-value">
            {raw.match(/../g)?.map((b, i) => (
              <span key={i}>{parseInt(b, 16).toString(2).padStart(8, '0')}</span>
            ))}
          </div>
        )}
      </div>
      <div className="detail-block">
        <h5>BEDEUTUNG</h5>
        <p>{field.documentation.description}</p>
        {field.documentation.reference && (
          <div className="standard-reference">
            <span>§</span>
            <span>{field.documentation.reference}</span>
          </div>
        )}
      </div>
      <div className="detail-block">
        <h5>GEPRÜFTE ANALYZER-WERTE</h5>
        {field.values.length ? (
          <dl className="validated-values">
            {field.values.map((value, i) => (
              <div key={i}>
                <dt>{value.name}</dt>
                <dd>{value.text}</dd>
              </div>
            ))}
          </dl>
        ) : (
          <p className="muted">
            Für dieses Feld ist kein eigener geprüfter Wert im Bericht enthalten. Zusammengesetzte
            Ergebnisse stehen gegebenenfalls beim übergeordneten Feld.
          </p>
        )}
      </div>
      {field.observation.kind === 'incomplete' && (
        <p className="inline-error">{field.observation.reason}</p>
      )}
      {diagnostics.map((d) => (
        <p key={d.id} className={d.severity === 'error' ? 'inline-error' : 'inline-notice'}>
          {d.message}
        </p>
      ))}
    </div>
  );
}

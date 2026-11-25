export interface Span {
  sourceId: string;
  offset: number;
  length: number;
}
export interface Protocol {
  id: string;
  name: string;
  scope: string;
}
export interface Source {
  id: string;
  name: string;
  hex: string;
  sensitive: boolean;
  origin: Span | null;
}
export interface FieldValue {
  name: string;
  text: string;
}
export type Observation =
  | { kind: 'parsed'; span: Span }
  | { kind: 'incomplete'; sourceId: string; offset: number; reason: string };
export interface Field {
  id: string;
  name: string;
  observation: Observation;
  values: FieldValue[];
  documentation: { description: string; reference: string | null };
  children: Field[];
}
export interface Diagnostic {
  id: string;
  severity: string;
  kind: string;
  message: string;
  fieldId: string | null;
  span: Span | null;
}
export type Outcome =
  | { kind: 'succeeded' }
  | { kind: 'failed'; diagnosticIds: string[] }
  | { kind: 'blocked'; causeId: string; reason: string };
export interface Step {
  id: string;
  name: string;
  input: Span;
  dependencies: string[];
  outcome: Outcome;
}
export interface MessageIdentity {
  name: string;
  description: string;
  direction: string;
  fieldId: string;
  reference: string;
}
export interface Report {
  schemaVersion: 1;
  protocol: Protocol;
  message?: MessageIdentity | null;
  allStepsSucceeded: boolean;
  sources: Source[];
  fields: Field[];
  diagnostics: Diagnostic[];
  steps: Step[];
}
export interface Candidate {
  id: string;
  inputId: string;
  confidence: string;
  evidence: string[];
  report: Report;
}
export interface Input {
  id: string;
  encoding: string;
  hex: string;
}
export interface AnalysisResponse {
  schemaVersion: 1;
  supportedProtocols: Protocol[];
  inputs: Input[];
  candidates: Candidate[];
  selectedCandidateId: string | null;
  messages: string[];
}

export function flattenFields(fields: Field[]): Field[] {
  return fields.flatMap((field) => [field, ...flattenFields(field.children)]);
}
export function fieldSpan(field: Field): Span {
  return field.observation.kind === 'parsed'
    ? field.observation.span
    : { sourceId: field.observation.sourceId, offset: field.observation.offset, length: 0 };
}
export function fieldAtByte(fields: Field[], sourceId: string, offset: number): Field | undefined {
  return flattenFields(fields)
    .reverse()
    .filter((field) => {
      const span = fieldSpan(field);
      return (
        span.sourceId === sourceId && span.offset <= offset && offset < span.offset + span.length
      );
    })
    .sort((a, b) => fieldSpan(a).length - fieldSpan(b).length)[0];
}

/** Validate persisted reports at the boundary; imported files never supply executable markup. */
export function readResponse(value: unknown): AnalysisResponse {
  const fail = (): never => {
    throw new Error(
      'Kein unterstützter Analysebericht (Schema-Version 1) oder beschädigte Berichtsstruktur.',
    );
  };
  const object = (x: unknown): Record<string, unknown> =>
    x !== null && typeof x === 'object' && !Array.isArray(x)
      ? (x as Record<string, unknown>)
      : fail();
  const text = (x: unknown): x is string => typeof x === 'string';
  const hex = (x: unknown): x is string =>
    text(x) && x.length <= 131072 && /^(?:[0-9a-fA-F]{2})*$/.test(x);
  const list = (x: unknown): unknown[] => (Array.isArray(x) ? x : fail());
  const idSet = (items: unknown[]): Set<string> => {
    const ids = items.map((x) => object(x).id);
    if (!ids.every(text) || new Set(ids).size !== ids.length) fail();
    return new Set(ids as string[]);
  };
  const protocol = (x: unknown) => {
    const p = object(x);
    if (![p.id, p.name, p.scope].every(text)) fail();
  };
  const root = object(value);
  if (root.schemaVersion !== 1) fail();
  list(root.supportedProtocols).forEach(protocol);
  const inputs = list(root.inputs);
  const inputIds = idSet(inputs);
  inputs.forEach((x) => {
    const p = object(x);
    if (!text(p.encoding) || !hex(p.hex)) fail();
  });
  const candidates = list(root.candidates);
  const candidateIds = idSet(candidates);
  for (const item of candidates) {
    const c = object(item);
    if (
      !text(c.inputId) ||
      !inputIds.has(c.inputId) ||
      !text(c.confidence) ||
      !list(c.evidence).every(text)
    )
      fail();
    const report = object(c.report);
    if (report.schemaVersion !== 1 || typeof report.allStepsSucceeded !== 'boolean') fail();
    protocol(report.protocol);
    const sources = list(report.sources);
    const sourceIds = idSet(sources);
    const lengths = new Map<string, number>();
    sources.forEach((x) => {
      const s = object(x);
      if (!text(s.name) || !hex(s.hex) || typeof s.sensitive !== 'boolean') fail();
      lengths.set(s.id as string, (s.hex as string).length / 2);
    });
    const span = (x: unknown) => {
      const s = object(x);
      if (
        !text(s.sourceId) ||
        !sourceIds.has(s.sourceId) ||
        !Number.isInteger(s.offset) ||
        !Number.isInteger(s.length) ||
        (s.offset as number) < 0 ||
        (s.length as number) < 0 ||
        (s.offset as number) + (s.length as number) > lengths.get(s.sourceId as string)!
      )
        fail();
    };
    sources.forEach((x) => {
      const s = object(x);
      if (s.origin !== null) span(s.origin);
    });
    const fieldIds = new Set<string>();
    let fieldCount = 0;
    const field = (x: unknown, depth: number) => {
      if (depth > 64 || ++fieldCount > 20000) fail();
      const f = object(x);
      if (!text(f.id) || fieldIds.has(f.id as string) || !text(f.name)) fail();
      fieldIds.add(f.id as string);
      const o = object(f.observation);
      if (o.kind === 'parsed') span(o.span);
      else if (o.kind === 'incomplete' && text(o.sourceId) && text(o.reason))
        span({ sourceId: o.sourceId, offset: o.offset, length: 0 });
      else fail();
      const d = object(f.documentation);
      if (!text(d.description) || !(d.reference === null || text(d.reference))) fail();
      list(f.values).forEach((v) => {
        const p = object(v);
        if (!text(p.name) || !text(p.text)) fail();
      });
      list(f.children).forEach((child) => field(child, depth + 1));
    };
    list(report.fields).forEach((f) => field(f, 0));
    if (report.message !== undefined && report.message !== null) {
      const m = object(report.message);
      if (
        ![m.name, m.description, m.direction, m.reference].every(text) ||
        !text(m.fieldId) ||
        !fieldIds.has(m.fieldId)
      )
        fail();
    }
    const diagnostics = list(report.diagnostics);
    const diagnosticIds = idSet(diagnostics);
    diagnostics.forEach((x) => {
      const d = object(x);
      if (
        ![d.severity, d.kind, d.message].every(text) ||
        !(d.fieldId === null || (text(d.fieldId) && fieldIds.has(d.fieldId)))
      )
        fail();
      if (d.span !== null) span(d.span);
    });
    const steps = list(report.steps);
    const stepIds = idSet(steps);
    steps.forEach((x) => {
      const s = object(x);
      const o = object(s.outcome);
      span(s.input);
      if (!text(s.name) || !list(s.dependencies).every((d) => text(d) && stepIds.has(d))) fail();
      if (o.kind === 'succeeded') return;
      if (
        o.kind === 'failed' &&
        list(o.diagnosticIds).every((d) => text(d) && diagnosticIds.has(d))
      )
        return;
      if (o.kind === 'blocked' && text(o.causeId) && stepIds.has(o.causeId) && text(o.reason))
        return;
      fail();
    });
  }
  if (
    !(
      root.selectedCandidateId === null ||
      (text(root.selectedCandidateId) && candidateIds.has(root.selectedCandidateId))
    ) ||
    !list(root.messages).every(text)
  )
    fail();
  return value as AnalysisResponse;
}

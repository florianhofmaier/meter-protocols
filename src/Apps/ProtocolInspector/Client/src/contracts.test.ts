import { describe, expect, it } from 'vitest';
import { fieldAtByte, readResponse, type AnalysisResponse, type Field } from './contracts';
import { documentHtml } from './report';

const field = (id: string, offset: number, length: number, children: Field[] = []): Field => ({
  id,
  name: `Field ${id}`,
  observation: { kind: 'parsed', span: { sourceId: '0', offset, length } },
  children,
  documentation: { description: 'Description', reference: null },
  values: [],
});
const fixture = (): AnalysisResponse => ({
  schemaVersion: 1,
  supportedProtocols: [{ id: 'test', name: 'Test', scope: 'Fixture' }],
  inputs: [{ id: 'input-0', encoding: 'Hex', hex: '010203' }],
  candidates: [
    {
      id: 'candidate',
      inputId: 'input-0',
      confidence: 'high',
      evidence: [],
      report: {
        schemaVersion: 1,
        protocol: { id: 'test', name: 'Test', scope: 'Fixture' },
        allStepsSucceeded: true,
        sources: [{ id: '0', name: 'Root', hex: '010203', sensitive: false, origin: null }],
        fields: [field('parent', 0, 3, [field('child', 1, 1)])],
        diagnostics: [],
        steps: [],
      },
    },
  ],
  selectedCandidateId: 'candidate',
  messages: [],
});

describe('protocol-neutral report', () => {
  it('finds the innermost covering field and keeps source boundaries', () => {
    const fields = fixture().candidates[0].report.fields;
    expect(fieldAtByte(fields, '0', 1)?.id).toBe('child');
    expect(fieldAtByte(fields, '0', 2)?.id).toBe('parent');
    expect(fieldAtByte(fields, 'other', 1)).toBeUndefined();
    expect(fieldAtByte(fields, '0', 3)).toBeUndefined();
  });
  it('round-trips a saved report', () =>
    expect(readResponse(JSON.parse(JSON.stringify(fixture())))).toEqual(fixture()));
  it('accepts optional message identity, validates its field and exports escaped text', () => {
    const response = fixture();
    const report = response.candidates[0].report;
    report.message = {
      name: 'SND-UD',
      description: '<script>send</script>',
      direction: 'Master → Zähler',
      fieldId: 'child',
      reference: 'Local standard',
    };
    expect(readResponse(JSON.parse(JSON.stringify(response)))).toEqual(response);
    const html = documentHtml(report);
    expect(html).toContain('SND-UD');
    expect(html).toContain('Master → Zähler');
    expect(html).toContain('&lt;script&gt;send&lt;/script&gt;');
    expect(html).not.toContain('<script>');
    report.message.fieldId = 'missing';
    expect(() => readResponse(response)).toThrow();
  });
  it('rejects broken schemas, duplicate identities and spans outside their source', () => {
    expect(() => readResponse({ schemaVersion: 9 })).toThrow();
    const broken = fixture();
    broken.candidates[0].report.fields.push(field('parent', 0, 1));
    expect(() => readResponse(broken)).toThrow();
    const invalid = fixture();
    invalid.candidates[0].report.fields = [field('outside', 2, 2)];
    expect(() => readResponse(invalid)).toThrow();
  });
  it('escapes untrusted report content in printable HTML and preserves exact numeric strings', () => {
    const report = fixture().candidates[0].report;
    report.fields[0].documentation.description = '<script>alert(1)</script>';
    report.fields[0].values = [{ name: 'Counter', text: '18446744073709551615' }];
    const html = documentHtml(report);
    expect(html).toContain('&lt;script&gt;');
    expect(html).not.toContain('<script>');
    expect(html).toContain('18446744073709551615');
  });
});

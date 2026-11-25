namespace Metering.Mbus.Analysis

open System
open System.Collections
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization
open Microsoft.FSharp.Reflection
open Metering.Common.Decoding.Parsers
open System.Xml
open System.Xml.Linq
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core

/// Only successful validation/operation results pass through this projection.
module private AnalysisValues =
    let private scalarOptions =
        let options =
            JsonSerializerOptions(
                NumberHandling = JsonNumberHandling.AllowNamedFloatingPointLiterals
            )

        options.Converters.Add(JsonStringEnumConverter())
        options

    let private primitive (value: obj) (typ: Type) =
        JsonSerializer.SerializeToNode(value, typ, scalarOptions)

    let span (span: SourceSpan) =
        let node = JsonObject()
        node["source"] <- JsonValue.Create(span.Source.Value)
        node["offset"] <- JsonValue.Create(span.Offset)
        node["length"] <- JsonValue.Create(span.Length)
        node

    let rec project (value: obj) : JsonNode =
        if isNull value then
            null
        else
            let typ = value.GetType()

            if typ = typeof<FieldId> then
                JsonValue.Create(FieldId.value (unbox value))
            elif typ = typeof<SourceId> then
                JsonValue.Create((unbox<SourceId> value).Value)
            elif typ = typeof<SourceSpan> then
                span (unbox value)
            elif typ = typeof<ReadOnlyMemory<byte>> then
                // Byte content is exported once per source, never as another raw-value cache.
                null
            elif typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Field<_>> then
                let node = JsonObject()
                node["id"] <- project (typ.GetProperty("Id").GetValue(value))
                node["span"] <- project (typ.GetProperty("Span").GetValue(value))

                if typ.GetGenericArguments()[0] <> typeof<ReadOnlyMemory<byte>> then
                    node["value"] <- project (typ.GetProperty("Value").GetValue(value))

                node
            elif typ = typeof<string> || typ.IsPrimitive || typ.IsEnum || typ = typeof<decimal> then
                primitive value typ
            elif value :? IEnumerable then
                let node = JsonArray()

                for item in value :?> IEnumerable do
                    node.Add(project item)

                node
            elif FSharpType.IsRecord(typ, true) then
                let node = JsonObject()

                for field in FSharpType.GetRecordFields(typ, true) do
                    node[JsonNamingPolicy.CamelCase.ConvertName(field.Name)] <-
                        project (field.GetValue value)

                node
            elif FSharpType.IsUnion(typ, true) then
                let case, fields = FSharpValue.GetUnionFields(value, typ, true)
                let node = JsonObject()
                node["case"] <- JsonValue.Create(case.Name)
                let names = case.GetFields()

                for index in 0 .. fields.Length - 1 do
                    node[JsonNamingPolicy.CamelCase.ConvertName(names[index].Name)] <-
                        project fields[index]

                node
            else
                primitive value typ

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MbusAnalysis =
    let private scalar value : JsonNode = JsonSerializer.SerializeToNode(value)

    let private array values : JsonNode =
        let node = JsonArray()

        for value in values do
            node.Add(value: JsonNode)

        node

    let private objectNode (properties: (string * JsonNode) list) : JsonNode =
        let node = JsonObject()

        for key, value in properties do
            node[key] <- value

        node

    let private issue (value: Issue) =
        objectNode [
            "fieldId", scalar (FieldId.value value.FieldId)
            "message", scalar value.Message
        ]

    let private parseError (error: ParserError) =
        objectNode [
            "source", scalar error.Source.Value
            "offset", scalar error.Pos
            "message", scalar error.Msg
        ]

    let private failure =
        function
        | ParseFailed error -> objectNode [ "kind", scalar "parse"; "error", parseError error ]
        | ValidationFailed errors ->
            objectNode [
                "kind", scalar "validation"
                "issues", errors |> Failures.toList |> List.map issue |> array
            ]
        | EncryptionFailed error -> objectNode [ "kind", scalar "protection"; "issue", issue error ]

    let private notice =
        function
        | Info value -> objectNode [ "severity", scalar "info"; "issue", issue value ]
        | Warning value -> objectNode [ "severity", scalar "warning"; "issue", issue value ]

    let rec private field (value: ParseField) =
        let result =
            match value.Result with
            | ParseFieldResult.Reading start ->
                objectNode [ "status", scalar "reading"; "start", AnalysisValues.span start ]
            | ParseFieldResult.Parsed span ->
                objectNode [ "status", scalar "parsed"; "span", AnalysisValues.span span ]
            | ParseFieldResult.Failed(start, error) ->
                objectNode [
                    "status", scalar "failed"
                    "start", AnalysisValues.span start
                    "error", parseError error
                ]

        objectNode [
            "id", scalar (FieldId.value value.Id)
            "name", scalar value.Name
            "result", result
            "children", value.Children |> List.map field |> array
        ]

    let private typedStep
        (name: string)
        (project: 'value -> JsonNode option)
        (step: AnalysisStep<'value>)
        =
        let result =
            match step.Result with
            | AnalysisStepResult.Succeeded value ->
                match project value with
                | None -> objectNode [ "status", scalar "succeeded" ]
                | Some value -> objectNode [ "status", scalar "succeeded"; "value", value ]
            | AnalysisStepResult.Failed error ->
                objectNode [ "status", scalar "failed"; "diagnostic", failure error ]
            | AnalysisStepResult.Blocked(cause, reason) ->
                objectNode [
                    "status", scalar "blocked"
                    "cause", scalar cause.Value
                    "reason", scalar reason
                ]

        let metadata = step.Metadata

        objectNode [
            "id", scalar metadata.Id.Value
            "name", scalar name
            "input", AnalysisValues.span metadata.Input
            "dependencies", metadata.Dependencies |> List.map (fun id -> scalar id.Value) |> array
            "result", result
            "notices", metadata.Notices |> List.map notice |> array
        ]

    let private step (value: MbusAnalysisStep) =
        let parse = typedStep value.Name (fun () -> None)

        let semantic step =
            typedStep value.Name (box >> AnalysisValues.project >> Some) step

        match value with
        | MbusAnalysisStep.DllParsing step -> parse step
        | MbusAnalysisStep.DllValidation step -> semantic step
        | MbusAnalysisStep.TelegramIdentification step -> semantic step
        | MbusAnalysisStep.CiParsing step -> parse step
        | MbusAnalysisStep.SelectionParsing step -> parse step
        | MbusAnalysisStep.SelectionValidation step -> semantic step
        | MbusAnalysisStep.EllParsing step -> parse step
        | MbusAnalysisStep.EllValidation step -> semantic step
        | MbusAnalysisStep.NwlParsing step -> parse step
        | MbusAnalysisStep.NwlValidation step -> semantic step
        | MbusAnalysisStep.AflParsing step -> parse step
        | MbusAnalysisStep.AflValidation step -> semantic step
        | MbusAnalysisStep.TplParsing step -> parse step
        | MbusAnalysisStep.TplValidation step -> semantic step
        | MbusAnalysisStep.ProtectionPreparation step -> semantic step
        | MbusAnalysisStep.ApplicationUnprotection step -> semantic step
        | MbusAnalysisStep.AplParsing step -> parse step
        | MbusAnalysisStep.AplValidation step -> semantic step
        | MbusAnalysisStep.MessageValidation step -> semantic step
        | MbusAnalysisStep.LinkUserData step -> parse step
        | MbusAnalysisStep.HigherLayers step -> parse step

    /// Source bytes are emitted once per source. External keys are not report inputs.
    let toJsonNode (report: MbusAnalysis) : JsonNode =
        let source (data: AnalysisSource) =
            let info = data.Info
            let bytes = data.Bytes

            objectNode [
                "id", scalar info.Id.Value
                "name", scalar info.Name
                "origin",
                info.Origin
                |> Option.map (AnalysisValues.span >> fun node -> node :> JsonNode)
                |> Option.defaultValue null
                "transform", AnalysisValues.project (box info.Transform)
                "length", scalar info.Length
                "sensitive", scalar info.Sensitive
                "bytes", scalar (Convert.ToHexString(bytes.Span))
            ]

        objectNode [
            "schemaVersion", scalar 1
            "isValid", scalar report.IsValid
            "sources", report.Sources |> List.map source |> array
            "fields", report.Trace.Fields |> List.map field |> array
            "steps", report.Steps |> List.map step |> array
        ]

    let toJson report =
        (toJsonNode report).ToJsonString(JsonSerializerOptions(WriteIndented = true))

    let toXml report =
        let rec element name (value: JsonNode) : XElement =
            let node = XElement(XName.Get name)

            match value with
            | null -> node.SetAttributeValue(XName.Get "nil", "true")
            | :? JsonObject as value ->
                for item in value do
                    node.Add(box (element item.Key item.Value))
            | :? JsonArray as value ->
                for item in value do
                    node.Add(box (element "item" item))
            | _ ->
                if value.GetValueKind() = JsonValueKind.String then
                    let text = value.GetValue<string>()

                    try
                        XmlConvert.VerifyXmlChars text |> ignore
                        node.Value <- text
                    with :? XmlException ->
                        node.SetAttributeValue(XName.Get "encoding", "json")
                        node.Value <- value.ToJsonString()
                else
                    node.Value <- value.ToJsonString()

            node

        let document = XDocument()
        document.Add(box (element "mbusAnalysis" (toJsonNode report)))
        document.ToString()

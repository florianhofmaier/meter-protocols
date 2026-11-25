namespace Metering.Common.Decoding.Decoders

open System
open System.Collections.Generic
open Metering.Common.Decoding.Parsers.Types

type InMemorySourceStore(createReader: ReaderFactory) =

    let buffers = Dictionary<SourceId, ReadOnlyMemory<byte>>()

    let mutable nextSourceId = 0

    let nextId () =
        let id = SourceId.create nextSourceId

        nextSourceId <- nextSourceId + 1
        id

    interface ISourceStore with

        /// <summary>Registers root bytes without copying them and assigns a new source identifier with the supplied name and sensitivity.</summary>
        member _.AddRoot name bytes sensitive =
            let id = nextId ()

            buffers.Add(id, bytes)

            {
                Id = id
                Name = name
                Origin = None
                Transform = SourceTransform.Root
                Length = bytes.Length
                Sensitive = sensitive
            }

        /// <summary>Registers derived bytes without copying them and assigns a new identifier with origin, transformation and sensitivity metadata.</summary>
        member _.AddDerived name origin transform bytes sensitive =
            let id = nextId ()

            buffers.Add(id, bytes)

            {
                Id = id
                Name = name
                Origin = Some origin
                Transform = transform
                Length = bytes.Length
                Sensitive = sensitive
            }

        /// <summary>Creates a fresh reader at offset zero for registered bytes; throws InvalidOperationException for an unknown source identifier.</summary>
        member _.CreateReader source =
            match buffers.TryGetValue source with
            | true, bytes -> createReader bytes 0

            | false, _ ->
                raise (InvalidOperationException $"Unknown decode source id {source.Value}")

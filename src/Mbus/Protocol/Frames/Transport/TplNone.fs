namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Mbus.Protocol.Frames

type TplNone =
	{
		Ci: NoneHeaderCiField
		Apl: ParsedField<AplDataRaw> option
	}

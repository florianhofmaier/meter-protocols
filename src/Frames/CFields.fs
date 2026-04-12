namespace Mbus.Frames

module CField =
    let rspUd = 0x08uy
    let setAcd cField = cField ||| 0x20uy
    let clearAcd cField = cField &&& 0xDFuy
    let setDfc cField = cField ||| 0x10uy
    let clearDfc cField = cField &&& 0xEFuy
    let ignoreFcb cField = cField &&& 0xDFuy
    let isReqUd2 cField = ignoreFcb cField = 0x5Buy
    let isSndUd cField = ignoreFcb cField = 0x53uy

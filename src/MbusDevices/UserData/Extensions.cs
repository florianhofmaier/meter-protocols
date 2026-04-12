using Microsoft.FSharp.Core;

namespace Mbus.UserData;

internal static class Extensions
{
    public static FSharpResult<TNew, TError> Map<T, TError, TNew>(
        this FSharpResult<T, TError> result,
        Func<T, TNew> mapper) =>
        result.IsError
            ? FSharpResult<TNew, TError>.NewError(result.ErrorValue)
            : FSharpResult<TNew, TError>.NewOk(mapper(result.ResultValue));

    public static T ValueOrThrow<T, TError>(
        this FSharpResult<T, TError> result,
        Func<TError, Exception> exceptionFactory) =>
        result.IsError
            ? throw exceptionFactory(result.ErrorValue)
            : result.ResultValue;

    public static TNew MapOrThrow<T, TError, TNew>(
        this FSharpResult<T, TError> result,
        Func<T, TNew> mapper,
        Func<TError, Exception> exceptionFactory) =>
        result.IsError
            ? throw exceptionFactory(result.ErrorValue)
            : mapper(result.ResultValue);

    public static ReadOnlySpan<byte> AsSpan(this BaseWriters.Core.WState wState) =>
        new (BaseWriters.Core.WStateModule.buf(wState), 0, BaseWriters.Core.WStateModule.pos(wState));

    public static MbusError ToMbusError(this BaseWriters.Core.WError error)
    {
        var ctxStr = error.Ctx switch
        {
            [] => "",
            _ => " Context: " + string.Join(".", error.Ctx.Reverse())
        };
        var msg = $"Error while writing {ctxStr} at position {error.Pos}: {error.Msg}";

        return new MbusError(msg);
    }
}

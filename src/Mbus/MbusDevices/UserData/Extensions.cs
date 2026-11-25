using Microsoft.FSharp.Core;

namespace Metering.Mbus.UserData;

internal static class Extensions
{
    internal static T ValueOrThrow<T>(this FSharpResult<T, string> result, string? parameterName = null) =>
        result.IsError
            ? throw new ArgumentException(result.ErrorValue, parameterName)
            : result.ResultValue;
}

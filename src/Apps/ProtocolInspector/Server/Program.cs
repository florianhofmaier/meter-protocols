using System.Text.Json;
using System.Text.Json.Serialization;
using System.Text.Json.Serialization.Metadata;
using System.Text.Json.Schema;
using Metering.Analysis;
using Metering.Mbus.AnalysisAdapter;

var builder = WebApplication.CreateBuilder(args);
builder.WebHost.ConfigureKestrel(options => options.Limits.MaxRequestBodySize = 2 * 1024 * 1024);
builder.Services.AddSingleton<IProtocolAnalyzer, MbusProtocolAnalyzer>();
builder.Services.AddSingleton<AnalysisService>();
builder.Services.ConfigureHttpJsonOptions(options =>
    options.SerializerOptions.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase)));
var app = builder.Build();

app.MapGet("/api/protocols", (AnalysisService service) => service.SupportedProtocols);
app.MapGet("/api/schema", () =>
{
    var options = new JsonSerializerOptions(JsonSerializerDefaults.Web)
    {
        TypeInfoResolver = new DefaultJsonTypeInfoResolver()
    };
    return Results.Json(options.GetJsonSchemaAsNode(typeof(AnalysisResponse)));
});
app.MapPost("/api/analyze", (AnalysisRequest request, AnalysisService service) =>
{
    try
    {
        if (request.Input is null) return Results.BadRequest(new { message = "Eine Texteingabe ist erforderlich." });
        return Results.Ok(service.Analyze(request.Input));
    }
    catch (ArgumentException error) { return Results.BadRequest(new { message = error.Message }); }
});
app.MapGet("/api/health", () => Results.Ok(new { status = "ok" }));

// Serve the independently built React client from the same origin in production.
var clientDist = Path.GetFullPath(Path.Combine(app.Environment.ContentRootPath, "../Client/dist"));
if (Directory.Exists(clientDist))
{
    var files = new Microsoft.Extensions.FileProviders.PhysicalFileProvider(clientDist);
    app.UseDefaultFiles(new DefaultFilesOptions { FileProvider = files });
    app.UseStaticFiles(new StaticFileOptions { FileProvider = files });
    app.MapFallback((HttpContext context) =>
        context.Request.Path.StartsWithSegments("/api")
            ? Results.NotFound()
            : Results.File(Path.Combine(clientDist, "index.html"), "text/html"));
}
app.Run();

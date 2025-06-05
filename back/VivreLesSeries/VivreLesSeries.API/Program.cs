using VivreLesSeries.Business;
using VivreLesSeries.Core.Repository;
using VivreLesSeries.Repository;
using VivreLesSeries.Repository.Context;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using Microsoft.EntityFrameworkCore;
using Microsoft.IdentityModel.Tokens;
using Microsoft.OpenApi.Models;
using System.Text;

var builder = WebApplication.CreateBuilder(args);

builder.Services.AddAuthentication(x =>
{
    x.DefaultAuthenticateScheme = JwtBearerDefaults.AuthenticationScheme;
    x.DefaultChallengeScheme = JwtBearerDefaults.AuthenticationScheme;
    x.DefaultScheme = JwtBearerDefaults.AuthenticationScheme;
}).AddJwtBearer(x =>
{
    x.TokenValidationParameters = new TokenValidationParameters
    {
        ValidateIssuer = true,
        ValidateAudience = true,
        ValidateLifetime = true,
        ValidateIssuerSigningKey = true,
        ValidIssuer = "VivreLesSeriesAPI",
        ValidAudience = "VivreLesSeriesClient",
        IssuerSigningKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes("Cl3OuJ3SaisPasQuo1M3ttr3MaisS3cur1s3DeFouAv3cD3sNombr3s"))
    };
});

builder.Services.AddAuthorization(options =>
{
    options.AddPolicy("isAdmin", p =>
        p.RequireClaim("role", "admin"));

    options.AddPolicy("AllowAnonymousGet", policy =>
        policy.RequireAssertion(context =>
            context.Resource is HttpContext httpContext &&
            httpContext.Request.Method == HttpMethods.Get
        ));
});

builder.Services.AddControllers();
// Learn more about configuring Swagger/OpenAPI at https://aka.ms/aspnetcore/swashbuckle
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen(option =>
{
    var jwtSecurityScheme = new OpenApiSecurityScheme
    {
        Scheme = "bearer",
        BearerFormat = "JWT",
        Name = "Authorization",
        In = ParameterLocation.Header,
        Type = SecuritySchemeType.Http,
        Description = "JWT Authorization header using the Bearer scheme.",
        Reference = new OpenApiReference
        {
            Id = JwtBearerDefaults.AuthenticationScheme,
            Type = ReferenceType.SecurityScheme
        }
    };

    option.AddSecurityDefinition("Bearer", new OpenApiSecurityScheme
    {
        In = ParameterLocation.Header,
        Description = "Please enter a valid token",
        Name = "Authorization",
        Type = SecuritySchemeType.Http,
        BearerFormat = "JWT",
        Scheme = "Bearer"
    });
    option.AddSecurityRequirement(new OpenApiSecurityRequirement
    {
        {
            new OpenApiSecurityScheme
            {
                Reference = new OpenApiReference
                {
                    Type=ReferenceType.SecurityScheme,
                    Id="Bearer"
                }
            },
            new string[]{}
        }
    });
});

builder.Services.AddScoped<IUserRepository, UserRepository>();

builder.Services.AddScoped<ISerieRepository, SerieRepository>();

builder.Services.AddScoped<SerieService>();

var dbPath = Path.Combine(Directory.GetCurrentDirectory(), "..", "VivreLesSeries.Repository", "Database", "VivreLesSeries.db");
dbPath = Path.GetFullPath(dbPath); 

if (!File.Exists(dbPath))
{
    Console.WriteLine($"Erreur : La base de données SQLite n'a pas été trouvée à l'emplacement {dbPath}.");
    throw new FileNotFoundException($"La base de données SQLite est introuvable : {dbPath}");
}

builder.Services.AddDbContext<UserSerieContext>(options =>
    options.UseSqlite($"Data Source={dbPath}"));

builder.Services.AddCors(options =>
{
    options.AddPolicy("AllowFrontDev", policy =>
    {
        policy.WithOrigins("http://localhost:5173")
              .AllowAnyHeader()
              .AllowAnyMethod()
              .AllowCredentials();
    });
});

builder.Services.AddMemoryCache();

var app = builder.Build();

if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();

}
app.UseCors("AllowFrontDev");
app.UseHttpsRedirection();

app.UseAuthentication();
app.UseAuthorization();

app.MapControllers();

app.Run();
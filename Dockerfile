# Backend API (BnfGen.Api) container for Render.com.
# Multi-stage: publish with the .NET SDK, then run on the slim ASP.NET runtime.

FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build
WORKDIR /src

# Restore/publish only the API (it pulls in BnfGen.Core via ProjectReference);
# BnfGen.Web is Fable-only and is never touched by the .NET toolchain.
COPY . .
RUN dotnet publish src/BnfGen.Api/BnfGen.Api.fsproj -c Release -o /app /p:UseAppHost=false

FROM mcr.microsoft.com/dotnet/aspnet:10.0 AS runtime
WORKDIR /app
COPY --from=build /app ./

# Render injects $PORT; the app reads it and binds 0.0.0.0:$PORT (default 8080).
EXPOSE 8080
ENTRYPOINT ["dotnet", "BnfGen.Api.dll"]

using Microsoft.AspNetCore.Mvc;
using VivreLesSeries.Business;
using VivreLesSeries.Repository;
using VivreLesSeries.Core.Repository;
using VivreLesSeries.Entity;
using VivreLesSeries.Entity.DTO;
using Microsoft.AspNetCore.Authorization;
using Microsoft.Extensions.Caching.Memory;
using VivreLesSeries.Core.Business;
using System.Security.Claims;
using VivreLesSeries.Repository.Context;

namespace VivreLesSeries.API.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    public class SeriesController : ControllerBase
    {
        private readonly IMemoryCache _cache;
        private readonly SerieService _serieService;

        public SeriesController(IMemoryCache cache, SerieService serieService)
        {
            _cache = cache;
            _serieService = serieService;
        }

        [AllowAnonymous]
        [HttpGet("list")]
        [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(List<Serie>))]
        [ProducesResponseType(StatusCodes.Status500InternalServerError, Type = typeof(ResponseMessage))]
        public async Task<IActionResult> GetTopRated()
        {
            const string cacheKey = "TopRatedSeries";

            if (!_cache.TryGetValue(cacheKey, out List<Serie> series))
            {
                series = await _serieService.GetTopRatedSeriesAsync();

                var cacheEntryOptions = new MemoryCacheEntryOptions()
                    .SetSlidingExpiration(TimeSpan.FromMinutes(10));

                _cache.Set(cacheKey, series, cacheEntryOptions);
            }

           
            if(series == null) {
                return StatusCode(500, new { message = "Erreur lors de la requête." });
            }
            return Ok(series);
        }

        [AllowAnonymous]
        [HttpGet("searchbyname/{name}")]
        [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(List<Serie>))]
        [ProducesResponseType(StatusCodes.Status400BadRequest, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status404NotFound, Type = typeof(ResponseMessage))]
        public async Task<IActionResult> GetSeriesByName(string name)
        {
            //Pas besoin de cache
            if (name.Length == 0)
            {
                return BadRequest(new ResponseMessage { Message = "Merci d'indiquer un nom de série" });
            }
            var series = await _serieService.GetSeriesByNameAsync(name);
            if (series == null)
            {
                return NotFound( new { message = "Aucune serie correspondante." });
            }
            return Ok(series);
        }
        [AllowAnonymous]
        [HttpGet("searchbyid/{id}")]
        [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(Serie))]
        [ProducesResponseType(StatusCodes.Status400BadRequest, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status404NotFound, Type = typeof(ResponseMessage))]
        public async Task<IActionResult> GetSerieById(int id)
        {
            string cacheKey = $"serie_{id}";
            if (!_cache.TryGetValue(cacheKey, out Serie serie))
            {
                if (id == 0)
                {
                    return BadRequest(new ResponseMessage { Message = "Problème d'identifiant" });
                }
                serie = await _serieService.GetSerieByIdAsync(id);
                if (serie == null)
                {
                    return NotFound(new { message = "Aucune serie correspondante." });
                }

                var cacheEntryOptions = new MemoryCacheEntryOptions()
                    .SetSlidingExpiration(TimeSpan.FromMinutes(10));

                _cache.Set(cacheKey, serie, cacheEntryOptions);
            }
            
            return Ok(serie);
        }

        [AllowAnonymous]
        [HttpGet("image/{id}")]
        [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(string))]
        [ProducesResponseType(StatusCodes.Status400BadRequest, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status404NotFound, Type = typeof(ResponseMessage))]
        public async Task<IActionResult> GetImageById(int id)
        {
            if (id == 0)
            {
                return BadRequest(new ResponseMessage { Message = "Problème d'identifiant" });
            }
            var path = await _serieService.GetImagePathByIdAsync(id);
            if (path == "")
            {
                return NotFound(new { message = "Aucune serie correspondante." });
            }
            return Ok(path);
        }

        [HttpPost("{serieId}/rating")]
        [Authorize]
        [ProducesResponseType(StatusCodes.Status201Created)]
        [ProducesResponseType(StatusCodes.Status401Unauthorized)]
        [ProducesResponseType(StatusCodes.Status400BadRequest, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status404NotFound, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status500InternalServerError, Type = typeof(ResponseMessage))]
        public async Task<IActionResult> AddRating(int serieId,[FromBody] RatingDto rating)
        {
            try
            {
                if (rating.Value < 0.5 || rating.Value > 10.0)
                {
                    return BadRequest(new ResponseMessage { Message = "La note doit être entre 0.5 et 10.0." });
                }
                var userId = int.Parse(User.FindFirst(ClaimTypes.NameIdentifier).Value);
                var newRating = new Rating
                {
                    Value = rating.Value,
                    CreatedAt = DateTime.UtcNow,
                    UserId = userId,
                    SerieId = serieId
                };
                var success = await _serieService.AddRatingAsync(serieId, newRating);
                if (success == System.Net.HttpStatusCode.Created)
                    return StatusCode(201);
                else if (success == System.Net.HttpStatusCode.NotFound)
                    return NotFound(new ResponseMessage { Message = "Le service ne peut pas trouver la série." });
                else if (success == System.Net.HttpStatusCode.BadRequest)
                    return BadRequest(new ResponseMessage { Message = "Il y a un problème dans la série ou dans la note donnée." });
            }
            catch (Exception ex)
            {
                return StatusCode(500, new { error = ex.Message, stackTrace = ex.StackTrace });
            }
            return StatusCode(500);


        }

        [HttpDelete("{serieId}/rating")]
        [Authorize]
        [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status400BadRequest, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status401Unauthorized)]
        [ProducesResponseType(StatusCodes.Status404NotFound, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status500InternalServerError, Type = typeof(ResponseMessage))]
        public async Task<IActionResult> DeleteRating(int serieId)
        {
            var success = await _serieService.DeleteRatingAsync(serieId);
            if (success == System.Net.HttpStatusCode.OK)
                return Ok(new ResponseMessage { Message = "Note supprimée avec succès." });
            else if (success == System.Net.HttpStatusCode.NotFound)
                return NotFound(new ResponseMessage { Message = "Le service ne peut pas trouver la série." });
            else if (success == System.Net.HttpStatusCode.BadRequest)
                return BadRequest(new ResponseMessage { Message = "Il y a un problème dans la série ou dans la note donnée." });
            else 
                return StatusCode(500, new ResponseMessage { Message = "Erreur lors de la suppression de la note." });
        }
    }
}
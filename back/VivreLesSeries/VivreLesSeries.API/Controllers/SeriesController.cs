using Microsoft.AspNetCore.Mvc;
using VivreLesSeries.Business;
using VivreLesSeries.Repository;
using VivreLesSeries.Core.Repository;
using VivreLesSeries.Entity;
using Microsoft.AspNetCore.Authorization;

namespace VivreLesSeries.API.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    public class SeriesController : ControllerBase
    {
        private readonly SerieService _serieService;

        public SeriesController()
        {
            ISerieRepository repo = new SerieRepository();
            _serieService = new SerieService(repo);
        }

        [AllowAnonymous]
        [HttpGet("list")]
        [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(List<Serie>))]
        [ProducesResponseType(StatusCodes.Status500InternalServerError, Type = typeof(ResponseMessage))]
        public async Task<IActionResult> GetTopRated()
        {
            var series = await _serieService.GetTopRatedSeriesAsync();
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
            if (id == 0)
            {
                return BadRequest(new ResponseMessage { Message = "Problème d'identifiant" });
            }
            var series = await _serieService.GetSerieByIdAsync(id);
            if( series == null)
            {
                return NotFound(new { message = "Aucune serie correspondante." });
            }
            return Ok(series);
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
        [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status401Unauthorized)]
        [ProducesResponseType(StatusCodes.Status400BadRequest, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status404NotFound, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status500InternalServerError, Type = typeof(ResponseMessage))]
        public async Task<IActionResult> AddRating(int serieId, string sessionId,[FromBody] Rating rating)
        {
            if (rating.Value < 0.5 || rating.Value > 10.0)
            {
                return BadRequest(new ResponseMessage { Message = "La note doit être entre 0.5 et 10.0." });
            }
            var success = await _serieService.AddRatingAsync(serieId, sessionId, rating.Value);
            if (success == System.Net.HttpStatusCode.OK)
                return Ok(new { message = "Note ajoutée avec succès." });
            else if (success == System.Net.HttpStatusCode.NotFound)
                return NotFound(new ResponseMessage { Message = "Le service ne peut pas trouver la série." });
            else if (success == System.Net.HttpStatusCode.BadRequest)
                return BadRequest(new ResponseMessage { Message = "Il y a un problème dans la série ou dans la note donnée." });
            else 
                return StatusCode(500, new { message = "Erreur non identifiée lors de l'ajout de la note." });
        }

        [HttpDelete("{serieId}/rating")]
        [Authorize]
        [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status400BadRequest, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status401Unauthorized)]
        [ProducesResponseType(StatusCodes.Status404NotFound, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status500InternalServerError, Type = typeof(ResponseMessage))]
        public async Task<IActionResult> DeleteRating(int serieId, string sessionId)
        {
            var success = await _serieService.DeleteRatingAsync(serieId, sessionId);
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
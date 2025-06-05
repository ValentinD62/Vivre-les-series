using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using System.Security.Claims;
using VivreLesSeries.Business;
using VivreLesSeries.Entity;
using VivreLesSeries.Entity.DTO;

namespace VivreLesSeries.API.Controllers
{
    [ApiController]
    [Route("api/comments")]
    public class CommentsController : ControllerBase
    {
        private readonly CommentService _service;
        public CommentsController(CommentService service) { _service = service; }

        [HttpGet("serie/{serieId}")]
        [AllowAnonymous]
        [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(Comment))]
        [ProducesResponseType(StatusCodes.Status500InternalServerError, Type = typeof(ResponseMessage))]
        public async Task<IActionResult> GetComments(int serieId)
        {
            var comments = await _service.GetComments(serieId);
            if (comments == null)
            {
                return StatusCode(500, new { message = "Problème à la création." });
            }
            return Ok(comments);
        }

        [HttpGet("serie/{serieId}/{userId}")]
        [Authorize]
        [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(Comment))]
        [ProducesResponseType(StatusCodes.Status404NotFound, Type = typeof(ResponseMessage))]
        public async Task<IActionResult> GetUserComments(int serieId, int userId)
        {
            var comment = await _service.GetUserComment(serieId, userId);
            return Ok(comment);
        }

        [HttpPost]
        [Authorize]
        [ProducesResponseType(StatusCodes.Status201Created, Type = typeof(Comment))]
        [ProducesResponseType(StatusCodes.Status400BadRequest)]
        [ProducesResponseType(StatusCodes.Status401Unauthorized)]
        [ProducesResponseType(StatusCodes.Status500InternalServerError)]
        public async Task<IActionResult> CreateComment([FromBody] CommentDto dto)
        {
            var comment = new Comment
            {
                Content = dto.Content,
                CreatedAt = DateTime.UtcNow,
                UserId = dto.UserId,
                SerieId = dto.SerieId
            };
            var created = await _service.CreateComment(comment);
            return CreatedAtAction(nameof(GetComments), new { serieId = dto.SerieId }, created);
        }

        [HttpDelete("{id}")]
        [Authorize]
        [ProducesResponseType(StatusCodes.Status204NoContent)]
        [ProducesResponseType(StatusCodes.Status401Unauthorized)]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        public async Task<IActionResult> DeleteComment(int id)
        {
            var success = await _service.DeleteComment(id);
            return success ? NoContent() : NotFound();
        }

        [HttpPut("{id}")]
        [Authorize]
        [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(ResponseMessage))]
        [ProducesResponseType(StatusCodes.Status401Unauthorized)]
        [ProducesResponseType(StatusCodes.Status404NotFound, Type = typeof(ResponseMessage))]
        public async Task<IActionResult> UpdateComment(int id, [FromBody] Comment updatedComment)
        {
            var success = await _service.UpdateCommentAsync(id, updatedComment.Content);
            if (!success)
                return NotFound(new { message = "Commentaire introuvable." });

            return Ok(new { message = "Commentaire mis à jour avec succès." });
        }
    }
}

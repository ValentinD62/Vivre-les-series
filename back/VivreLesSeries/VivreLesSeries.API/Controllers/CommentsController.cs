using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using System.Security.Claims;
using VivreLesSeries.Core.Business;
using VivreLesSeries.Entity;

namespace VivreLesSeries.API.Controllers
{
    [ApiController]
    [Route("api/comments")]
    public class CommentsController : ControllerBase
    {
        private readonly ICommentService _service;
        public CommentsController(ICommentService service) { _service = service; }

        [HttpGet("serie/{serieId}")]
        [AllowAnonymous]
        public async Task<IActionResult> GetComments(int serieId)
        {
            var comments = await _service.GetComments(serieId);
            return Ok(comments);
        }

        [HttpPost]
        [Authorize]
        public async Task<IActionResult> CreateComment([FromBody] CommentDto dto)
        {
            var userId = int.Parse(User.FindFirst(ClaimTypes.NameIdentifier).Value);
            var comment = new Comment
            {
                Content = dto.Content,
                CreatedAt = DateTime.UtcNow,
                UserId = userId,
                SerieId = dto.SerieId
            };
            var created = await _service.CreateComment(comment);
            return CreatedAtAction(nameof(GetComments), new { serieId = dto.SerieId }, created);
        }

        [HttpDelete("{id}")]
        [Authorize]
        public async Task<IActionResult> DeleteComment(int id)
        {
            var success = await _service.DeleteComment(id);
            return success ? NoContent() : NotFound();
        }

        public class CommentDto
        {
            public string Content { get; set; }
            public int SerieId { get; set; }
        }
    }
}

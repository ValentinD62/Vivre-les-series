using VivreLesSeries.Core.Business;
using VivreLesSeries.Core.Repository;
using VivreLesSeries.Entity;

namespace VivreLesSeries.Business
{
    public class CommentService : ICommentService
    {
        private readonly ICommentRepository _repository;
        public CommentService(ICommentRepository repository) { _repository = repository; }

        public Task<IEnumerable<Comment>> GetComments(int serieId)
        {
            return _repository.GetCommentsBySerieId(serieId);
        }
        public Task<Comment?> GetUserComment(int serieId, int userId)
        {
            return _repository.GetUserComment(serieId, userId);
        }
        public Task<Comment> CreateComment(Comment comment)
        {
            return _repository.AddComment(comment);
        }
        public Task<bool> DeleteComment(int commentId)
        {
            return _repository.DeleteComment(commentId);
        }
        public async Task<bool> UpdateCommentAsync(int id, string content)
        {
            var comment = await _repository.GetByIdAsync(id);
            if (comment == null) return false;

            comment.Content = content;
            await _repository.UpdateAsync(comment);
            return true;
        }
    }
}

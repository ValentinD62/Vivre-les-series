using VivreLesSeries.Entity;

namespace VivreLesSeries.Core.Repository
{
    public interface ICommentRepository
    {
        Task<IEnumerable<Comment>> GetCommentsBySerieId(int serieId);
        Task<Comment> AddComment(Comment comment);
        Task<bool> DeleteComment(int commentId);
        Task<Comment?> GetByIdAsync(int id);
        Task UpdateAsync(Comment comment);
    }
}

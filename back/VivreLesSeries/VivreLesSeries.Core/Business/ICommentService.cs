using VivreLesSeries.Entity;

namespace VivreLesSeries.Core.Business
{
    public interface ICommentService
    {
        Task<IEnumerable<Comment>> GetComments(int serieId);
        Task<Comment> CreateComment(Comment comment);
        Task<bool> DeleteComment(int commentId);
    }
}

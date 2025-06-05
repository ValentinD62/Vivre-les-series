using Microsoft.EntityFrameworkCore;
using VivreLesSeries.Core.Repository;
using VivreLesSeries.Entity;
using VivreLesSeries.Repository.Context;

namespace VivreLesSeries.Repository
{
    public class CommentRepository : ICommentRepository
    {
        private readonly UserSerieContext _context;
        public CommentRepository(UserSerieContext context) { _context = context; }

        public async Task<IEnumerable<Comment>> GetCommentsBySerieId(int serieId)
        {
            return await _context.Comments
                .Where(c => c.SerieId == serieId)
                .ToListAsync();
        }

        public async Task<Comment?> GetUserComment(int serieId, int userId)
        {
            var commentExists = await _context.Comments
                .FirstOrDefaultAsync(c => c.SerieId == serieId && c.UserId == userId);
            return commentExists;
        }

        public async Task<Comment> AddComment(Comment comment)
        {
            _context.Comments.Add(comment);
            await _context.SaveChangesAsync();
            return comment;
        }

        public async Task<bool> DeleteComment(int commentId)
        {
            var comment = await _context.Comments.FindAsync(commentId);
            if (comment == null) return false;
            _context.Comments.Remove(comment);
            await _context.SaveChangesAsync();
            return true;
        }

        public async Task<Comment?> GetByIdAsync(int id)
        {
            return await _context.Comments.FindAsync(id);
        }

        public async Task UpdateAsync(Comment comment)
        {
            _context.Comments.Update(comment);
            await _context.SaveChangesAsync();
        }
    }
}

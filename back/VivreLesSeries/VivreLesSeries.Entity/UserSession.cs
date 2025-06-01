namespace VivreLesSeries.Entity
{
    public class UserSession
    {
        public int Id { get; set; }
        public string TmdbSessionId { get; set; }
        public DateTime CreatedAt { get; set; }

        public int UserId { get; set; }
        public User User { get; set; }
    }
}
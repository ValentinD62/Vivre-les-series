namespace VivreLesSeries.Entity
{
    public class User
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public string Password { get; set; }
        public ICollection<UserSession> Sessions { get; set; } = new List<UserSession>();
    }
}

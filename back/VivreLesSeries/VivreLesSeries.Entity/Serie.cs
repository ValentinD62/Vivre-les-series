using System.Reflection;
using System.Text.Json.Serialization;

namespace VivreLesSeries.Entity
{
    public class Serie
    {
        [JsonPropertyName("id")]
        public int Id { get; set; }

        [JsonPropertyName("name")]
        public string Name { get; set; }

        [JsonPropertyName("vote_average")]
        public double VoteAverage { get; set; }

        [JsonPropertyName("first_air_date")]
        public string ReleasingDate {  get; set; }

        [JsonPropertyName("overview")]
        public string Description { get; set; }

        [JsonPropertyName("backdrop_path")]
        public string ImagePath { get; set; }
    }
}

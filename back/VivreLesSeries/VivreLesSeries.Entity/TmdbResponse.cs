using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.Json.Serialization;
using System.Threading.Tasks;

namespace VivreLesSeries.Entity
{
    public class TmdbResponse
    {
        [JsonPropertyName("results")]
        public List<Serie> Results { get; set; }

        [JsonPropertyName("backdrops")]
        public List<Image> ImagesResults { get; set; }
    }
}

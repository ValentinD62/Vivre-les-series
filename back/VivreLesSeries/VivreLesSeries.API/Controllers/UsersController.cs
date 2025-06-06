using Microsoft.AspNetCore.Mvc;
using VivreLesSeries.Core.Repository;
using VivreLesSeries.Core.Business;
using VivreLesSeries.Entity;
using VivreLesSeries.Entity.DTO;
using Microsoft.IdentityModel.Tokens;
using System.IdentityModel.Tokens.Jwt;
using System.Security.Claims;
using System.Text;
using Microsoft.EntityFrameworkCore;
using VivreLesSeries.Repository.Context;
using Microsoft.AspNetCore.Authorization;

[ApiController]
[Route("api/[controller]")]
public class UsersController : ControllerBase
{
    private readonly IUserRepository _userRepository;
    private readonly IUserSessionService _userSessionService;
    private readonly UserSerieContext _userSerieContext;

    public UsersController(IUserRepository userRepository, UserSerieContext context)
    {
        _userRepository = userRepository;
        _userSerieContext = context;
    }

    [HttpGet("search/{userId}")]
    [Authorize]
    [ProducesResponseType(StatusCodes.Status401Unauthorized)]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(User))]
    [ProducesResponseType(StatusCodes.Status404NotFound, Type = typeof(ResponseMessage))]
    public async Task<IActionResult> SearchUserById(int userId)
    {
        var user = _userRepository.GetUserById(userId);
        if (user == null)
        {
            return NotFound(new { message = "L'utilisateur n'a pas été trouvé" });
        }
        return Ok(user.Result);
    }

    [HttpPost("login")]
    [AllowAnonymous]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(Object))]
    [ProducesResponseType(StatusCodes.Status400BadRequest, Type = typeof(ResponseMessage))]
    public IActionResult Login([FromBody] UserDto dto)
    {
        var user = _userRepository.LoginAsync(dto.Name, dto.Password);

        if (user?.Result?.Name == null)
            return BadRequest("Identifiants invalides.");
        var tokenHandler = new JwtSecurityTokenHandler();
        var key = Encoding.UTF8.GetBytes("Cl3OuJ3SaisPasQuo1M3ttr3MaisS3cur1s3DeFouAv3cD3sNombr3s");

        var tokenDescriptor = new SecurityTokenDescriptor
        {
            Subject = new ClaimsIdentity(new[]
            {
                new Claim(ClaimTypes.NameIdentifier, user.Id.ToString()),
                new Claim(ClaimTypes.Name, dto.Name)
            }),
            Expires = DateTime.UtcNow.AddHours(2),
            Issuer = "VivreLesSeriesAPI",
            Audience = "VivreLesSeriesClient",
            SigningCredentials = new SigningCredentials(new SymmetricSecurityKey(key), SecurityAlgorithms.HmacSha256Signature)
        };

        var token = tokenHandler.CreateToken(tokenDescriptor);
        var jwt = tokenHandler.WriteToken(token);
        return Ok(new { token = jwt, id = user?.Result?.Id });
    }

    [HttpPost("createuser")]
    [AllowAnonymous]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(Object))]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError, Type = typeof(ResponseMessage))]
    public async Task<IActionResult> CreateUser([FromBody] UserDto userDto)
    {
        if (await _userSerieContext.Users.AnyAsync(u => u.Name == userDto.Name))
            return BadRequest("Nom d'utilisateur déjà utilisé.");

        var user = new User
        {
            Name = userDto.Name,
            Password = userDto.Password
        };
        var createdUser = await _userRepository.CreateUserAsync(user);
        var tokenHandler = new JwtSecurityTokenHandler();
        var key = Encoding.UTF8.GetBytes("Cl3OuJ3SaisPasQuo1M3ttr3MaisS3cur1s3DeFouAv3cD3sNombr3s");
        var tokenDescriptor = new SecurityTokenDescriptor
        {
            Subject = new ClaimsIdentity(new[]
            {
                new Claim(ClaimTypes.Name, user.Name)
            }),
            Expires = DateTime.UtcNow.AddHours(1),
            Issuer = "VivreLesSeriesAPI",
            Audience = "VivreLesSeriesClient",
            SigningCredentials = new SigningCredentials(new SymmetricSecurityKey(key), SecurityAlgorithms.HmacSha256Signature)
        };
        var token = tokenHandler.CreateToken(tokenDescriptor);
        var jwt = tokenHandler.WriteToken(token);

        return Ok(new { token = jwt, id = createdUser.Id });
    }

    [HttpPost("{userId}/sessions")]
    [Authorize]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(Object))]
    [ProducesResponseType(StatusCodes.Status401Unauthorized)]
    public async Task<IActionResult> AddSession(int userId, [FromBody] UserSessionDto sessionDto)
    {
        var session = await _userRepository.AddSessionAsync(userId, sessionDto.TmdbSessionId);
        return Ok(new { session.Id, session.TmdbSessionId, session.CreatedAt });
    }

    [HttpPost("{userId}/sessions/link")]
    [Authorize]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(Object))]
    [ProducesResponseType(StatusCodes.Status401Unauthorized)]
    public async Task<IActionResult> LinkTmdbSession(int userId, [FromBody] LinkSessionDto dto)
    {
        var session = await _userSessionService.LinkTmdbSessionAsync(userId, dto.RequestToken);
        return Ok(new { session.Id, session.TmdbSessionId, session.CreatedAt });
    }

}

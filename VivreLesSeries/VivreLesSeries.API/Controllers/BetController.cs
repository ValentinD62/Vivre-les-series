using BetCo.Core.Business;
using BetCo.Entity;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace BetCo.API.Controllers
{
    [Authorize]
    [Route("api/[controller]")]
    [ApiController]
    public class BetController : ControllerBase
    {
        private readonly IBetService _betService;

        public BetController(IBetService betService)
        {
            _betService = betService;
        }

        [AllowAnonymous]
        [HttpGet]
        public IEnumerable<Bet> GetAll()
        {
            return _betService.RetrieveAll();
        }

        [HttpGet]
        [Route("{Id}")]
        public Bet GetById(int Id)
        {
            return _betService.RetrieveById(Id);
        }

        [HttpPost]
        [Route("")]
        public Bet Post(Bet bet)
        {
            return _betService.Add(bet);
        }

        [HttpPut]
        [Route("{Id}")]
        public Bet Put(int Id, Bet bet)
        {
            return _betService.Update(Id, bet);
        }

        [HttpDelete]
        [Authorize(Policy = "isAdmin")]
        [Route("{Id}")]
        public bool Delete(int Id)
        {
            return _betService.Remove(Id);
        }
    }
}

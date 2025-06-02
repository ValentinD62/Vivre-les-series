import { customElement, property, state } from "lit/decorators.js";
import { html, css, LitElement, unsafeCSS, nothing } from "lit";
import UserCSS from "./user.scss?inline";


@customElement('user-form')
export class UserComponent extends LitElement {

    @property({attribute: "is-visible-user-pop-up", type: Boolean })
    isVisibleUserPopUp: boolean = false;

    @state()
    hasAccount: boolean = false;

    handleHasAccount() {
        this.hasAccount = !this.hasAccount;
    }

    renderComponent() {
        return html`
            <div class="user-form-container">
                <div class="user-form-container__title-div">
                    ${this.hasAccount ? html`Connexion` : html`Inscription`}
                </div>
                <div class="user-form-container__form-div">
                    <form class="user-form-container__form" method=${this.hasAccount ? "GET" : "POST"}>
                        <div class="user-form-container__form-input">
                            <div class="user-form-container__form-input-pseudo">
                                <label for="pseudo">Pseudo</label>
                                <input type="text" id="pseudo" name="pseudo" placeholder="Pseudo" required autofocus>
                            </div>
                            <div class="user-form-container__form-input-mdp">
                                <label for="password">Mot de passe</label>
                                <input type="password" id="password" name="password" placeholder="Mot de passe" required>
                            </div>
                        </div>
                        <div class="user-form-container__form-btn">
                            <button type="submit" id="submit">
                                ${this.hasAccount ? "Se connecter" : "S'inscrire"}
                            </button>
                        </div>
                    </form>
                </div>
                <div class="user-form-container__footer">
                    <span>
                    ${this.hasAccount ? html `Pas de compte ? <span @click=${this.handleHasAccount} class="user-form-container__footer-link">Inscrivez-vous</span>` :
                                        html `Déjà un compte ? <span @click=${this.handleHasAccount} class="user-form-container__footer-link">Connectez-vous</span>`}
                    </span>
                </div>
            </div>`;
    }

    isRenderComponent() {
        return this.isVisibleUserPopUp ? this.renderComponent() : nothing;
    }

    render() {
        return html `${this.isRenderComponent()}`;
    }
    static styles = css`${unsafeCSS(UserCSS)}`;
}

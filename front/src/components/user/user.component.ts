import { customElement, property, state } from "lit/decorators.js";
import { html, css, LitElement, unsafeCSS, nothing } from "lit";
import UserCSS from "./user.scss?inline";
import { postCreateUser, postLoginUser } from "../../API/main.ts";
import { escapeHtml, validateUserInput } from "../../shared/function.ts";


@customElement('user-form')
export class UserComponent extends LitElement {

    @property({attribute: "is-visible-user-pop-up", type: Boolean })
    isVisibleUserPopUp: boolean = false;

    @state()
    hasAccount: boolean = false;

    @state()
    errorMessage: string = "";

    override connectedCallback(): void {
        super.connectedCallback();
        document.addEventListener("mousedown", this.handleOutsideClick);
    }

    override disconnectedCallback(): void {
        super.disconnectedCallback();
        document.removeEventListener("mousedown", this.handleOutsideClick);
    }

    private handleOutsideClick = (event: MouseEvent): void => {
        const path = event.composedPath();
        // Cherche si la modale (this) fait partie du chemin du clic
        if (!path.includes(this)) {
            this.isVisibleUserPopUp = false;
        }
    };

    handleHasAccount() {
        this.hasAccount = !this.hasAccount;
    }

    getConnected() {
        this.dispatchEvent(new CustomEvent('connected'));
    }

    handleInscription(event: Event) {
        event.preventDefault();
        const form = event.target as HTMLFormElement;
        let pseudo = (form.elements.namedItem("pseudo") as HTMLInputElement).value;
        let password = (form.elements.namedItem("password") as HTMLInputElement).value;

        if (pseudo && password) {
            // Appel API pour l'inscription
            pseudo = escapeHtml(pseudo)
            password = escapeHtml(password)
            console.log(`Inscription: ${pseudo}, ${password}`);
            if (validateUserInput(pseudo, password) !== "") {
                this.errorMessage = validateUserInput(pseudo, password);
                return;
            }
            postCreateUser(pseudo, password)
                .then((response) => {
                    localStorage.setItem('username', pseudo);
                    localStorage.setItem('token', response.token);
                    localStorage.setItem('userId', response.id);
                    this.hasAccount = true;
                    this.isVisibleUserPopUp = false; // Ferme la pop-up après l'inscription
                    this.getConnected();
                })
                .catch((error) => {
                    console.error("Erreur lors de la création de l'utilisateur :", error);
                    this.errorMessage = "Ce pseudo est déjà utilisé. Veuillez en choisir un autre.";
                    return;
                });
        } else {
            this.errorMessage = "Pseudo et mot de passe requis pour l'inscription.";
        }
    }

    handleConnexion(event: Event) {
        event.preventDefault();
        const form = event.target as HTMLFormElement;
        let pseudo = (form.elements.namedItem("pseudo") as HTMLInputElement).value;
        let password = (form.elements.namedItem("password") as HTMLInputElement).value;

        if (pseudo && password) {
            // Call the API to connect the user
            pseudo = escapeHtml(pseudo)
            password = escapeHtml(password)
            console.log(`Connexion: ${pseudo}, ${password}`);
            if (validateUserInput(pseudo, password) !== "") {
                this.errorMessage = validateUserInput(pseudo, password);
                return;
            }
            postLoginUser(pseudo, password)
                .then((response) => {
                    localStorage.setItem('username', pseudo);
                    localStorage.setItem('token', response.token);
                    localStorage.setItem('userId', response.id);
                    this.isVisibleUserPopUp = false;
                    this.getConnected();
                })
                .catch((error) => {
                    console.error("Erreur lors de la connexion :", error);
                    this.errorMessage = "Erreur lors de la connexion. Veuillez vérifier vos identifiants.";
                    return;
                })
        } else {
            console.error("Pseudo et mot de passe sont requis pour la connexion.");
            this.errorMessage = "Pseudo et mot de passe sont requis pour la connexion.";
        }
    }

    renderComponent() {
        return html`
            <div class="user-form-container">
                <div class="user-form-container__title-div">
                    ${this.hasAccount ? html`Connexion` : html`Inscription`}
                </div>
                <div class="user-form-container__form-div">
                    <div class="user-form-container__error-div">
                        ${this.errorMessage}
                    </div>
                    <form class="user-form-container__form" method=${this.hasAccount ? "GET" : "POST"} @submit=${this.hasAccount ? this.handleConnexion : this.handleInscription}>
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

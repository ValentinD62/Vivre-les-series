import {customElement, property} from "lit/decorators.js";
import {html, css, LitElement, unsafeCSS, nothing, PropertyValues} from "lit";
import UserCSS from "./user.scss?inline";

@customElement('user-form')
export class UserComponent extends LitElement {

    @property({attribute: "is-visible-user-pop-up", type: Boolean })
    isVisibleUserPopUp: boolean = false;

    @property({attribute: "is-connected", type: Boolean})
    isConnexion: boolean = false;

    protected updated(_changedProperties: PropertyValues): void {
        if (_changedProperties.has('isVisibleUserPopUp')) {
            console.log('isVisibleUserPopUp changed:', this.isVisibleUserPopUp);
        }
    }

    renderComponent() {
        console.log(this.isVisibleUserPopUp);
        return this.isVisibleUserPopUp ? html`
            <div class="user-form-container">
                <div class="user-form-container__title-div">
                    ${this.isConnexion ? html`Connexion` : html`Inscription`}
                </div>
                oui
            </div>
        ` : nothing;
    }

    render() {
        return html `${this.renderComponent()}`;
    }
    static styles = css`${unsafeCSS(UserCSS)}`;
}
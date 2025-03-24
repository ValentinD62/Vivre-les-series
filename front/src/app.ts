import "./assets/variable.scss";
import "./my-element.ts"
import "./partials/header/header.component.ts"
import "./partials/body/body.component.ts"
import "./components/user/user.component.ts"
import {customElement, state} from "lit/decorators.js";
import {css, html, LitElement, unsafeCSS} from "lit";
import AppCSS from "./app.scss?inline";

@customElement('app-component')
export class AppComponent extends LitElement {
    @state()
    isVisibleUserPopUp = false;

    showUserPopUp() {
        this.isVisibleUserPopUp = !this.isVisibleUserPopUp;
    }

    render() {
        return html`
            <div class="app-container">
                <header-component @showUserPage="${this.showUserPopUp}"></header-component>
                <user-form ?is-visible-user-pop-up="${this.isVisibleUserPopUp}"></user-form>
                <body-component></body-component>
            </div>
        `;
    }
    static styles = css`${unsafeCSS(AppCSS)}`;
}

declare global {
    interface HTMLElementTagNameMap {
        'app-component': AppComponent;
    }
}
import "./assets/variable.scss";
import "./my-element.ts"
import "./partials/header/header.component.ts"
import "./components/user/user.component.ts"
import {customElement, state} from "lit/decorators.js";
import { html, LitElement } from "lit";

@customElement('app-component')
export class AppComponent extends LitElement {
    @state()
    isVisibleUserPopUp = false;

    showUserPopUp() {
        this.isVisibleUserPopUp = !this.isVisibleUserPopUp;
    }

    render() {
        console.log(this.isVisibleUserPopUp);
        return html`
            <header-component @showUserPage="${this.showUserPopUp}"></header-component>
            <user-form is-visible-user-pop-up="${this.isVisibleUserPopUp}"></user-form>
        `;
    }
}

declare global {
    interface HTMLElementTagNameMap {
        'app-component': AppComponent;
    }
}
import { customElement, state } from "lit/decorators.js";
import {html, css, LitElement, unsafeCSS } from "lit";
import SearchBarCSS from "./search-bar.scss?inline";

@customElement('search-bar')
export class SearchBarComponent extends LitElement {
    @state()
    isShowBar = false;
    showBar() {
        this.isShowBar = !this.isShowBar;
    }

    @state()
    inputValue: string = "";

    private handleInput(event: InputEvent) {
        const target = event.target as HTMLInputElement;
        this.inputValue = target.value;
        this.dispatchEvent(new CustomEvent("updateSearch", {detail: {value: this.inputValue}}));
    }

    renderShowbar() {
        return this.isShowBar ?
            html`<div class="search-bar-container__div-bar">
                            <input class="search-bar-container__input" 
                                   type="text"
                                   placeholder="Taper un nom de série"
                                   .value=${this.inputValue}
                                   @input=${this.handleInput}
                                   autofocus />
                        </div>` :
            html`<div class="search-bar-container__div-bar"></div>`;
    }
    render() {
        return html`
            <div class="search-bar-container">
                ${this.renderShowbar()}
                <div class="search-bar-container__div-icon" @click="${this.showBar}">
                    <img src="icons_loupe.png" class="search-bar-container__img" alt="loupe">
                </div>
            </div>
        `;
    }
    static styles = css`${unsafeCSS(SearchBarCSS)}`;
}

declare global {
    interface HTMLElementTagNameMap {
        'search-bar': SearchBarComponent
    }
}

import {customElement, state} from "lit/decorators.js";
import {css, html, LitElement, unsafeCSS} from "lit";
import {HeaderComponent} from "../header/header.component.ts";
import BodyCSS from "./body.scss?inline";
import {getSeriesById} from "../../API/main.ts";

@customElement('body-component')
export class BodyComponent extends LitElement {

    @state()
    series: { id?: number; name?: string; vote_average?: number; first_air_date?: string; overview?: string; backdrop_path?: string } = {};

    getSerie(){
        // Simulate fetching series data
        getSeriesById(1396).then((series) => {
            this.series = series;
        }).catch((error) => {
            console.error("Error fetching series data:", error);
            this.series = {
                name: "Error loading series",
                backdrop_path: "/src/assets/no_image.png"
            };
        })
    }

    render() {
        this.getSerie();
        return html`
            <div class="body-container">
                <div class="body-container__div-latest-img" style="background-image: url(${this.series.backdrop_path});">
                    <div class="body-container__div-latest-text">
                        <h2>Notre recommendation : </h2>
                        <h2> ${this.series.name}</h2>
                    </div>
                </div>
            </div>
        `;
    }
    static styles = css`${unsafeCSS(BodyCSS)}`;
}

declare global {
    interface HTMLElementTagNameMap {
        'header-component': HeaderComponent
    }
}

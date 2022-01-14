import plotly.graph_objects as go
import pandas as pd
import glob




def main():
    table_files = glob.glob("out/structure_learning/*/*/*.csv")

    htmls = []

        

    for f in table_files:
        df = pd.read_csv(f)

        if "Unnamed: 0" in df.columns:
            df.drop(columns=["Unnamed: 0"], inplace=True)

        table = go.Table(
            header=dict(
                values=list(df.columns),
                fill_color="paleturquoise",
                align="left",
            ),
            cells=dict(
                values=[df[c] for c in df.columns],
                fill_color="lavender",
                align="left",
            )
        )

        fig = go.Figure(data=[table])

        # set title to file_name:
        fig.update_layout(title=f)

        # add html to list
        html = fig.to_html(full_html=False, include_plotlyjs="cdn")
        htmls.append(html)

    out_file = "out/structure_learning/table_overview.html"

    with open(out_file, "w") as f:
        f.write("<html><body>")
        for html in htmls:
            f.write(html)
        f.write("</body></html>")

if __name__ == "__main__":
    main()

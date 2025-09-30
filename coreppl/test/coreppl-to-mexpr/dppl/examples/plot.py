import json
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter

plt.style.use("tableau-colorblind10")

CB_BLACK = "#000000"
CB_ORANGE = "#E69F00"
CB_SKY_BLUE = "#56B4E9"
CB_BLUISH_GREEN = "#009E73"
CB_YELLOW = "#F0E442"
CB_BLUE = "#0072B2"
CB_VERMILLION = "#D55E00"
CB_REDDISH_PURPLE = "#CC79A7"

BLUE = CB_BLUE
RED = CB_VERMILLION
GREEN = CB_BLUISH_GREEN
GRAY = "tab:gray"


def post_process_weights_samples(weights, samples):
    w = np.asarray(weights)
    s = np.asarray(samples)
    finite_mask = np.isfinite(w)
    w = w[finite_mask]
    m = max(w)
    w = np.exp(w - m)
    s = s[finite_mask]
    return w, s


def set_grid(ax):
    ax.grid(
        True,
        which="major",
        linestyle="-",
        linewidth=1.5,
        color="gray",
        alpha=0.5,
    )
    ax.grid(
        True,
        which="minor",
        linestyle="--",
        linewidth=1.5,
        color="gray",
        alpha=0.2,
    )
    ax.minorticks_on()


def plot_hist(ax, samples, weights, bins):
    ax.hist(
        samples,
        bins=bins,
        density=True,
        weights=weights,
        color=GREEN,
        edgecolor="black",
    )
    set_grid(ax)


def plot_scalar_dist(file_name):
    try:
        with open(file_name, "r") as file:
            data = json.load(file)
            weights, samples = post_process_weights_samples(
                data["weights"], data["samples"]
            )
            plt.rcParams.update({"font.size": 18})
            fig, ax = plt.subplots(figsize=(8, 6))
            ax.set_xlabel
            fig.suptitle(file_name)
            plot_hist(ax, samples, weights, 200)
            fig.tight_layout()
            fig.savefig(file_name.replace("-run.json", ".pdf"))
    except FileNotFoundError:
        print(f"{file_name} not found")


def plot_trace(
    ax, xs, samples, trueTrace, prey_label, pred_label, yminofs, ymaxofs
):
    num_samples = len(samples)
    alpha = min(0.1, 5 / num_samples) if num_samples > 0 else 0.1
    plt.rcParams.update({"font.size": 22})

    def plot(ys, color, label):
        if i == 0:
            ax.plot(
                xs,
                ys,
                color=color,
                alpha=alpha,
                label=label,
            )
        else:
            ax.plot(xs, ys, color=color, alpha=alpha, label=None)

    for i in range(len(samples)):
        ys = samples[i].transpose()
        plot(ys[0], BLUE, None)
    ax.plot(
        xs,
        trueTrace.transpose()[0],
        color="black",
        linestyle="dashed",
        label=prey_label,
        linewidth=2,
    )
    for i in range(len(samples)):
        ys = samples[i].transpose()
        plot(ys[1], RED, None)
    ax.plot(
        xs,
        trueTrace.transpose()[1],
        color="black",
        linestyle="dashdot",
        label=pred_label,
        linewidth=2,
    )
    ax.set_ylim(
        bottom=np.min(trueTrace) - yminofs,
        top=np.max(trueTrace) + ymaxofs,
    )
    ax.set_xlabel("x")
    legend = ax.legend()
    # legend.legendPatch.set_facecolor("lightgray")
    for lh in legend.legend_handles:
        lh.set_alpha(1)
    set_grid(ax)


def plot_trace_dist(file_name, prey_label, pred_label, yminofs, ymaxofs):
    try:
        with open(file_name, "r") as file:
            data = json.load(file)
            xs = np.asarray(data["xs"])
            trueTrace = np.asarray(data["trueTrace"])
            weights, samples = post_process_weights_samples(
                data["weights"], data["trace"]
            )

            fig, ax = plt.subplots(figsize=(6, 6))
            plot_trace(
                ax,
                xs,
                samples,
                trueTrace,
                prey_label,
                pred_label,
                yminofs,
                yminofs,
            )
            fig.tight_layout()
            fig.savefig(file_name.replace("-run.json", ".pdf"))
    except FileNotFoundError:
        print(f"{file_name} not found")


def plot_sens_dist(file_name):
    try:
        with open(file_name, "r") as file:
            data = json.load(file)
            xs = np.asarray(data["xs"])
            samples = np.asarray(data["samples"])
            plt.rcParams.update({"font.size": 24})
            fig, ax = plt.subplots(
                1, 2, figsize=(20, 4), constrained_layout=True
            )

            colors = [CB_BLUE, CB_ORANGE, CB_BLACK]

            def plot(j):
                for i in range(len(samples)):
                    ys = samples[i][j].transpose()
                    for k in range(len(ys)):
                        ax[j].plot(
                            xs,
                            ys[k],
                            alpha=5 * min(1, 1 / len(samples)),
                            color=colors[k % len(colors)],
                        )
                        set_grid(ax[j])
                        ax[j].set_xlabel(r"$x$")

            plot(0)
            plot(1)
            ax[0].set_ylabel(r"$s_{\theta}(x)$")
            # fig.tight_layout()
            fig.savefig(file_name.replace("-run.json", ".pdf"))
    except FileNotFoundError:
        print(f"{file_name} not found")


plot_scalar_dist("bayesian-parameter-estimation-run.json")
plot_scalar_dist("bayesian-parameter-estimation-ivp-solution-run.json")
plot_scalar_dist("bayesian-parameter-estimation-ivp-sensitivity-run.json")

plot_trace_dist(
    "bayesian-parameter-estimation-ivp-solution-trace-run.json",
    "prey  density",
    "pred. density",
    1,
    4,
)
plot_trace_dist(
    "bayesian-parameter-estimation-ivp-sensitivity-trace-run.json",
    "prey  density sens.",
    "pred. density sens.",
    5,
    7,
)

try:
    file1 = "bayesian-parameter-estimation-run.json"
    with open(file1, "r") as file1:
        file2 = "bayesian-parameter-estimation-ivp-solution-trace-run.json"
        with open(file2, "r") as file2:
            file3 = (
                "bayesian-parameter-estimation-ivp-sensitivity-trace-run.json"
            )
            with open(file3, "r") as file3:
                data1 = json.load(file1)
                data2 = json.load(file2)
                data3 = json.load(file3)

                weights1, samples1 = post_process_weights_samples(
                    data1["weights"], data1["samples"]
                )
                xs2 = np.asarray(data2["xs"])
                trueTrace2 = np.asarray(data2["trueTrace"])
                weights2, samples2 = post_process_weights_samples(
                    data2["weights"], data2["trace"]
                )
                xs3 = np.asarray(data2["xs"])
                trueTrace3 = np.asarray(data3["trueTrace"])
                weights3, samples3 = post_process_weights_samples(
                    data3["weights"], data3["trace"]
                )
                plt.rcParams.update({"font.size": 24})
                fig, ax = plt.subplots(
                    1,
                    3,
                    figsize=(20, 5),
                    gridspec_kw={"width_ratios": [1, 2, 2]},
                    constrained_layout=True,
                )
                ax[0].set_xlim(0, 2)
                plot_hist(ax[0], samples1, weights1, 200)
                ax[0].set_xlabel(r"$\theta$")
                current_ticks = ax[0].get_xticks()
                new_ticks = np.sort(np.unique(np.append(current_ticks, 1.5)))
                ax[0].set_xticks(new_ticks)

                plot_trace(
                    ax[1],
                    xs2,
                    samples2,
                    trueTrace2,
                    r"$y_1(x)$",
                    r"$y_2(x)$",
                    1,
                    4,
                )
                plot_trace(
                    ax[2],
                    xs3,
                    samples3,
                    trueTrace3,
                    r"$\frac{d}{d\theta}y_1(x)$",
                    r"$\frac{d}{d\theta}y_2(x)$",
                    5,
                    7,
                )
                fig.savefig("bayesian-parameter-estimation-combined.pdf")

except FileNotFoundError:
    print(f"All files not found")

plot_sens_dist("ode-sensitivites-two-methods-scalar-run.json")
plot_sens_dist("ode-sensitivites-two-methods-run.json")

try:
    file = "rode-run.json"
    with open(file, "r") as file:
        data = json.load(file)
        xs = np.asarray(data["xs"])
        ys = np.asarray(data["ys"])
        ws = np.asarray(data["ws"])
        plt.rcParams.update({"font.size": 24})
        fig, ax = plt.subplots(1, 2, figsize=(20, 4), constrained_layout=True)

        linestyle = ["-", "-.", "--", ":", (0, (5, 5))]

        for i in range(len(ys)):
            ax[0].plot(
                xs, ys[i], linewidth=2, linestyle=linestyle[i % len(linestyle)]
            )
            ax[1].plot(
                xs, ws[i], linewidth=2, linestyle=linestyle[i % len(linestyle)]
            )

        # plt.subplots_adjust(wspace=2.5)  # Increase horizontal space
        set_grid(ax[0])
        set_grid(ax[1])
        ax[0].set_xlabel(r"$x$")
        ax[0].set_ylabel(r"$y(x)$")
        ax[1].set_xlabel(r"$x$")
        ax[1].set_ylabel(r"$w(x)$")
        # fig.tight_layout()
        fig.savefig("rode.pdf")
except FileNotFoundError:
    print(f"{file} not found")

try:
    file = "tumor-inhibitor-rode-run.json"
    with open(file, "r") as file:
        data = json.load(file)
        xs = np.asarray(data["xs"])
        sol = np.asarray(data["sol"])
        sens = np.asarray(data["sens"])
        plt.rcParams.update({"font.size": 24})
        fig, ax = plt.subplots(1, 2, figsize=(12, 8), constrained_layout=True)

        linestyle = ["-", "-.", "--", ":", (0, (5, 5))][::-1]

        def plot(ax, ys, i, j, label, color):
            ax.plot(
                xs,
                ys[i].transpose()[j],
                alpha=5 * min(1, 1 / len(sol)),
                color=color,
                linewidth=2,
                linestyle=linestyle[i % len(linestyle)],
                label=label if i == 0 else None,
            )

        def set_axis(ax1, ax2):
            formatter = ScalarFormatter(useMathText=True)
            formatter.set_scientific(True)
            formatter.set_powerlimits((0, 0))

            ax1.yaxis.set_major_formatter(formatter)
            ax2.yaxis.set_major_formatter(formatter)

            ax1.set_zorder(ax2.get_zorder() + 1)
            ax1.patch.set_visible(False)
            ax1.grid(True)
            ax2.grid(True, linestyle="--")

        def set_legend(ax1, ax2, loc):
            lines1, labels1 = ax1.get_legend_handles_labels()
            lines2, labels2 = ax2.get_legend_handles_labels()
            ax1.legend(lines1 + lines2, labels1 + labels2, loc=loc)

        ax0tw = ax[0].twinx()

        for i in range(len(sol)):
            plot(ax[0], sol, i, 0, r"$C(t)$", CB_BLUE)
            plot(ax0tw, sol, i, 1, r"$P(t)$", CB_ORANGE)
            plot(ax0tw, sol, i, 2, r"$I(t)$", CB_BLACK)

        ax[0].set_ylabel("Cancer cells", color=GRAY)
        ax0tw.set_ylabel("Promoters and Inhibitors", color=GRAY)
        ax[0].set_yticks([0, 10, 20, 30, 40, 50])
        ax0tw.set_yticks([0, 500, 1000, 1500, 2000])

        set_axis(ax[0], ax0tw)
        set_legend(ax[0], ax0tw, "lower left")

        ax1tw = ax[1].twinx()

        for i in range(len(sens)):
            plot(ax[1], sens, i, 0, r"$\partial_{a_I} C(t)$", CB_BLUE)
            plot(ax1tw, sens, i, 1, r"$\partial_{a_I} P(t)$", CB_ORANGE)
            plot(ax1tw, sens, i, 2, r"$\partial_{a_I} I(t)$", CB_BLACK)

        ax[1].set_ylabel("Cancer cells", color=GRAY)
        ax1tw.set_ylabel("Promoters and Inhibitors", color=GRAY)
        # ax[0].set_yticks([0, 10, 20, 30, 40, 50])
        # ax0tw.set_yticks([0, 500, 1000, 1500, 2000])

        set_axis(ax[1], ax1tw)
        set_legend(ax[1], ax1tw, "lower left")
        fig.savefig("tumor-inhibitor-rode-overview.pdf")

except FileNotFoundError:
    print(f"{file} not found")

try:
    file = "tumor-inhibitor-rode-run.json"
    with open(file, "r") as file:
        data = json.load(file)
        xs = np.asarray(data["xs"])
        sol = np.asarray(data["sol"])
        sens = np.asarray(data["sens"])
        ws = np.asarray(data["ws"])
        plt.rcParams.update({"font.size": 24})
        fig, ax = plt.subplots(3, 2, figsize=(10, 8), constrained_layout=True)

        linestyle = ["-", "-.", "--", ":", (0, (5, 5))][::-1]
        markers = ["o", "s", "^", "*", "D"]

        def plot(ax, ys, i, j, label, color):
            ax.plot(
                xs,
                ys[i].transpose()[j],
                alpha=5 * min(1, 1 / len(sol)),
                # color=color,
                linewidth=3,
                linestyle=linestyle[i % len(linestyle)],
                # marker=markers[i % len(markers)],
                label=label if i == 0 else None,
            )

        def set_axis(ax):
            formatter = ScalarFormatter(useMathText=True)
            formatter.set_scientific(True)
            formatter.set_powerlimits((0, 0))
            ax.yaxis.set_major_formatter(formatter)

        def set_legend(ax, loc):
            lines, labels = ax.get_legend_handles_labels()
            ax.legend(lines, labels, loc=loc)

        for i in range(len(sol)):
            plot(ax[0][0], sol, i, 0, r"$C(t)$", CB_BLUE)
            plot(ax[1][0], sol, i, 1, r"$P(t)$", CB_ORANGE)
            plot(ax[2][0], sol, i, 2, r"$I(t)$", CB_BLACK)

        for i in range(len(sens)):
            plot(ax[0][1], sens, i, 0, r"$\partial_{a_I} C(t)$", CB_BLUE)
            plot(ax[1][1], sens, i, 1, r"$\partial_{a_I} P(t)$", CB_ORANGE)
            plot(ax[2][1], sens, i, 2, r"$\partial_{a_I} I(t)$", CB_BLACK)

        for aa in ax:
            for a in aa:
                set_axis(a)
                set_grid(a)

        xticks = [0, 30, 60]

        ax[0][0].set_yticks([30, 40])
        ax[0][0].set_xticks(xticks)
        ax[0][0].set_xticklabels([])
        ax[1][0].set_yticks([1400, 1600])
        ax[1][0].set_xticks(xticks)
        ax[1][0].set_xticklabels([])
        ax[2][0].set_yticks([1000, 1200])
        ax[2][0].set_xticks(xticks)

        ax[0][1].set_yticks([0, -100])
        ax[0][1].set_xticks(xticks)
        ax[0][1].set_xticklabels([])
        ax[1][1].set_yticks([0, -4000])
        ax[1][1].set_xticks(xticks)
        ax[1][1].set_xticklabels([])
        ax[2][1].set_yticks([0, 4000])
        ax[2][1].set_xticks(xticks)

        ax[0][0].set_ylabel(r"$C(t)$")
        ax[1][0].set_ylabel(r"$P(t)$")
        ax[2][0].set_ylabel(r"$I(t)$")

        ax[0][1].set_ylabel(r"$\frac{\partial}{\partial a_I} C(t)$")
        ax[1][1].set_ylabel(r"$\frac{\partial}{\partial a_I} P(t)$")
        ax[2][1].set_ylabel(r"$\frac{\partial}{\partial a_I} I(t)$")

        ax[2][0].set_xlabel(r"$t$")
        ax[2][1].set_xlabel(r"$t$")

        fig.align_ylabels(ax)
        fig.savefig("tumor-inhibitor-rode.pdf")

        fig, ax = plt.subplots(1, 1, figsize=(6, 3), constrained_layout=True)
        for i in range(len(ws)):
            plot(ax, ws, i, 0, r"$W_t$", CB_SKY_BLUE)

        set_axis(ax)
        set_grid(ax)

        ax.set_yticks([-20, 0, 20])
        ax.set_xticks(xticks)

        ax.set_ylabel(r"$W_t$")
        ax.set_xlabel(r"$t$")

        fig.align_ylabels(ax)
        fig.savefig("tumor-inhibitor-rode-wiener.pdf")

except FileNotFoundError:
    print(f"{file} not found")

plt.show()

import json
import numpy as np
import matplotlib.pyplot as plt


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
        linewidth=0.5,
        color="gray",
        alpha=0.5,
    )
    ax.grid(
        True,
        which="minor",
        linestyle="--",
        linewidth=0.5,
        color="gray",
        alpha=0.2,
    )
    ax.minorticks_on()


def plot_hist(ax, samples, weights, bins, left, right):
    ax.hist(samples, bins=bins, density=True, weights=weights, color="blue")
    ax.set_xlim(left=left, right=right)
    set_grid(ax)


try:
    file = "baysian-parameter-estimation.json"
    with open("baysian-parameter-estimation.json", "r") as file:
        data = json.load(file)
        weights, samples = post_process_weights_samples(
            data["weights"], data["samples"]
        )
        plt.rcParams.update({"font.size": 18})
        fig, ax = plt.subplots(figsize=(4, 4))
        plot_hist(ax, samples, weights, 200, 0, 2)
        ax.set_xticks(list(ax.get_xticks()) + [1.5])
        fig.tight_layout()
except FileNotFoundError:
    print(f"{file} not found")

try:
    file = "baysian-parameter-estimation-sensitivities.json"
    with open(file, "r") as file:
        data = json.load(file)
        xs = np.asarray(data["xs"])
        trueSens = np.asarray(data["trueSens"])
        weights, samples = post_process_weights_samples(
            data["weights"], data["samples"]
        )
        plt.rcParams.update({"font.size": 14})
        fig, ax = plt.subplots(figsize=(4, 4))
        for i in range(len(samples)):
            color = "blue"
            alpha = min(1, weights[i])
            if i == 0:
                ax.plot(
                    xs,
                    samples[i],
                    color=color,
                    alpha=alpha,
                    label="posterior",
                )
            else:
                ax.plot(xs, samples[i], color=color, alpha=alpha, label=None)
        ax.plot(
            xs,
            trueSens,
            color="red",
            linestyle="dashed",
            label="true derivative",
        )
        ax.set_ylim(bottom=np.min(trueSens) - 5, top=np.max(trueSens) + 5)
        ax.set_xlabel("x")
        legend = ax.legend()
        legend.legendPatch.set_facecolor("lightgray")
        for lh in legend.legend_handles:
            lh.set_alpha(1)
        set_grid(ax)
        fig.tight_layout()
except FileNotFoundError:
    print(f"{file} not found")

try:
    file = "ode-sensitivites.json"
    with open("ode-sensitivites.json", "r") as file:
        data = json.load(file)
        xs = np.asarray(data["xs"])
        samples = np.asarray(data["samples"])
        plt.rcParams.update({"font.size": 24})
        fig, ax = plt.subplots(1, 2, figsize=(20, 4), constrained_layout=True)

        def plot(j):
            for i in range(len(samples)):
                ys = samples[i][j].transpose()
                ax[j].plot(
                    xs, ys[0], alpha=5 * min(1, 1 / len(samples)), color="green"
                )
                ax[j].plot(
                    xs, ys[1], alpha=5 * min(1, 1 / len(samples)), color="red"
                )
                set_grid(ax[j])
                ax[j].set_xlabel(r"$x$")

        plot(0)
        plot(1)
        ax[0].set_ylabel(r"$s_{\theta}(x)$")
        # fig.tight_layout()
except FileNotFoundError:
    print(f"{file} not found")

try:
    file = "rode-faster.json"
    with open("rode-faster.json", "r") as file:
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
        ax[1].set_ylabel(r"$W(x)$")
        # fig.tight_layout()
except FileNotFoundError:
    print(f"{file} not found")

plt.show()

# Execute at the rsc folder.
#
# The script creates the figures of the README file. The figures are also used
# by the plot method of the package as visualizations for the market models.

import matplotlib.pyplot as plt
import numpy as np
import PIL.ImageOps
from PIL import Image
from scipy.optimize import fsolve

colors = {"identification": "blue",
          "market-clearing": "#9936B1",
          "market-force": "#FF4500",
          "shortage": "#0ECBA3"}


def quantity_space():
    return np.linspace(0.0, 1.0, num=100)


def demand(q):
    return 1 - q


def supply(q):
    return q


def equilibrium(qinit):
    def system(q):
        return demand(q) - supply(q)

    qem = fsolve(system, qinit)
    pem = demand(qem)
    return (qem, pem)


def demanded_quantity(p):
    def system(q):
        return p - demand(q)

    return fsolve(system, demand(p))


def supplied_quantity(p):
    def system(q):
        return p - supply(q)

    return fsolve(system, supply(p))


def get_axes(fig):
    ymin = -0.05
    ymax = 1.05
    xmin = -0.05
    xmax = 1.05

    ax = fig.add_axes((0.05, 0.05, 0.96, 0.96))
    ax.spines["bottom"].set_alpha(0.0)
    ax.spines["left"].set_alpha(0.0)
    ax.spines["right"].set_alpha(0.0)
    ax.spines["top"].set_alpha(0.0)
    ax.set_xticks([])
    ax.set_yticks([])

    # get width and height of axes object to compute
    # matching arrowhead length and width
    dps = fig.dpi_scale_trans.inverted()
    bbox = ax.get_window_extent().transformed(dps)
    width, height = bbox.width, bbox.height

    # manual arrowhead width and length
    hw = 1.0 / 50.0 * (ymax - ymin)
    hl = 1.0 / 50.0 * (xmax - xmin)
    lw = 1.0  # axis line width
    ohg = 0.3  # arrow overhang

    # compute matching arrowhead length and width
    yhw = hw / (ymax - ymin) * (xmax - xmin) * height / width
    yhl = hl / (xmax - xmin) * (ymax - ymin) * width / height

    # draw x and y axis
    ax.arrow(
        xmin,
        ymin,
        0.98 * (xmax - xmin),
        0.0,
        fc="k",
        ec="k",
        lw=lw,
        head_width=hw,
        head_length=hl,
        overhang=ohg,
        length_includes_head=True,
        clip_on=True,
    )
    ax.annotate(
        "Quantity",
        xy=(xmax, 0.0),
        xytext=(xmax - 0.15, -0.1),
        ha="center",
    )

    ax.arrow(
        xmin,
        ymin,
        0.0,
        0.98 * (ymax - ymin),
        fc="k",
        ec="k",
        lw=lw,
        head_width=yhw,
        head_length=yhl,
        overhang=ohg,
        length_includes_head=True,
        clip_on=True,
    )
    ax.annotate(
        "Price",
        xy=(0.0, ymax),
        xytext=(-0.08, ymax - 0.18),
        ha="center",
        rotation=90,
    )

    return ax


def equilibrium_figure():
    fig_em = plt.figure()
    # with plt.xkcd():
    ax = get_axes(fig_em)

    q = quantity_space()

    ax.annotate(
        "Only Equilibrium\nis observed",
        xy=(equilibrium(q[50])[0], equilibrium(q[50])[1] * 1.05),
        arrowprops=dict(arrowstyle="->", color=colors["market-clearing"]),
        xytext=(0.5, 0.80),
        ha="center",
        color=colors["market-clearing"],
    )

    ax.plot(q, demand(q), color=colors["market-force"])
    ax.plot(q, supply(q), color=colors["market-force"])
    ax.plot(
        equilibrium(q[50])[0],
        equilibrium(q[50])[1],
        "o",
        mfc="none",
        color=colors["market-clearing"],
    )

    ax.annotate("Demand", xy=(0.88, 0.22), ha="center",
                color=colors["market-force"])
    ax.annotate("Supply", xy=(0.88, 0.75), ha="center",
                color=colors["market-force"])

    return fig_em


def diseq_basic_figure():
    fig_bm = plt.figure()
    # with plt.xkcd():
    ax = get_axes(fig_bm)

    q = quantity_space()

    (qeq, peq) = equilibrium(q[50])
    psh = peq * 0.8
    dsh = demanded_quantity(psh)
    ssh = supplied_quantity(psh)
    pdm = peq * 0.6
    dq = demanded_quantity(pdm)
    psp = peq * 0.3
    sq = supplied_quantity(psp)

    ax.plot(q, demand(q), color=colors["market-force"])
    ax.plot(q, supply(q), color=colors["market-force"])
    ax.plot(q[q < qeq], demand(q[q < qeq]), "-", alpha=0.8, ls="dashdot",
            color=colors["identification"])
    ax.plot(q[q < qeq], supply(q[q < qeq]), "-",
            alpha=0.8, ls="dashdot", color=colors["identification"])
    ax.plot([dsh, ssh], [psh, psh], color=colors["shortage"])
    ax.plot(dsh, psh, "o", mfc="none", color=colors["shortage"])
    ax.plot(ssh, psh, "o", mfc="none", color=colors["shortage"])

    ax.annotate(
        "There exist\nshortages & surpluses",
        xy=((dsh + ssh) / 2, psh * 0.95),
        arrowprops=dict(arrowstyle="->", color=colors["shortage"]),
        xytext=(0.5, 0.10),
        ha="center",
        color=colors["shortage"],
    )
    ax.annotate(
        "",
        xy=(pdm, dq * 0.97),
        arrowprops=dict(arrowstyle="->", color=colors["identification"]),
        xytext=(0.2, 0.59),
    )
    ax.annotate(
        "We observe\npoints here,\nbut do not know\nthe market side.",
        xy=(pdm * 0.8, sq * 1.7),
        arrowprops=dict(arrowstyle="->", color=colors["identification"]),
        xytext=(0.2, 0.40),
        ha="center",
        color=colors["identification"],
    )

    ax.annotate("Demand", xy=(0.88, 0.22), ha="center",
                color=colors["market-force"])
    ax.annotate("Supply", xy=(0.88, 0.75), ha="center",
                color=colors["market-force"])

    return fig_bm


def diseq_directional_figure():
    fig_dm = plt.figure()
    # with plt.xkcd():
    ax = get_axes(fig_dm)

    q = quantity_space()

    (qeq, peq) = equilibrium(q[50])
    psh = peq * 0.8
    ssh = supplied_quantity(psh)
    psu = peq * 1.4
    dsu = demanded_quantity(psu)

    ax.plot(q, demand(q), color=colors["market-force"])
    ax.plot(q, supply(q), color=colors["market-force"])
    ax.plot(q[q < qeq], supply(q[q < qeq]), "-",
            ls="dashdot", color=colors["identification"])
    ax.plot(q[q < qeq], demand(q[q < qeq]), "-",
            ls="dashdot", color=colors["identification"])

    ax.annotate(
        "Observed if\n$\Delta P > 0$",
        xy=(ssh, psh * 0.95),
        arrowprops=dict(arrowstyle="->", color=colors["identification"]),
        xytext=(0.4, 0.10),
        ha="center",
        color=colors["identification"],
    )

    ax.annotate(
        "Observed if\n$\Delta P < 0$",
        xy=(dsu, psu * 1.05),
        arrowprops=dict(arrowstyle="->", color=colors["identification"]),
        xytext=(0.3, 0.90),
        ha="center",
        color=colors["identification"],
    )

    ax.annotate("Demand", xy=(0.88, 0.22), ha="center",
                color=colors["market-force"])
    ax.annotate("Supply", xy=(0.88, 0.75), ha="center",
                color=colors["market-force"])

    return fig_dm


def diseq_deterministic_adjustment_figure():
    fig_da = plt.figure()
    # with plt.xkcd():
    ax = get_axes(fig_da)

    q = quantity_space()

    (qeq, peq) = equilibrium(q[50])
    psh = peq * 0.3
    dsh = demanded_quantity(psh)
    ssh = supplied_quantity(psh)
    psu = peq * 0.7
    dsu = demanded_quantity(psu)
    ssu = supplied_quantity(psu)

    ax.plot(q, demand(q), color=colors["market-force"])
    ax.plot(q, supply(q), color=colors["market-force"])
    ax.plot(q[q < qeq], supply(q[q < qeq]), "-",
            ls="dashdot", color=colors["identification"])
    ax.plot(q[q < qeq], demand(q[q < qeq]), "-",
            ls="dashdot", color=colors["identification"])
    ax.plot([dsh, ssh], [psh, psh], color=colors["shortage"])
    ax.plot([dsu, ssu], [psu, psu], color=colors["shortage"])
    ax.plot(dsh, psh, "o", mfc="none", color=colors["shortage"])
    ax.plot(ssh, psh, "o", mfc="none", color=colors["shortage"])
    ax.plot(dsu, psu, "o", mfc="none", color=colors["shortage"])
    ax.plot(ssu, psu, "o", mfc="none", color=colors["shortage"])

    ax.annotate(
        "$\Delta P$",
        xy=((dsh + ssh) * 0.7, psh * 1.1),
        arrowprops=dict(arrowstyle="->", color=colors["shortage"]),
        xytext=(0.82, 0.38),
        color=colors["shortage"],
    )
    ax.annotate(
        "",
        xy=(0.89, 0.36),
        arrowprops=dict(arrowstyle="<-", color=colors["shortage"]),
        xytext=(0.89, 0.44),
    )
    ax.annotate(
        "",
        xy=(0.91, 0.36),
        arrowprops=dict(arrowstyle="<-", color=colors["shortage"]),
        xytext=(0.91, 0.44),
    )

    ax.annotate(
        "$\Delta P$",
        xy=((dsu + ssu) * 0.55, psu * 1.07),
        arrowprops=dict(arrowstyle="->", color=colors["shortage"]),
        xytext=(0.7, 0.55),
        color=colors["shortage"],
    )
    ax.annotate(
        "",
        xy=(0.78, 0.54),
        arrowprops=dict(arrowstyle="<-", color=colors["shortage"]),
        xytext=(0.78, 0.62),
        color=colors["shortage"],
    )

    ax.annotate("Demand", xy=(0.88, 0.22), ha="center",
                color=colors["market-force"])
    ax.annotate("Supply", xy=(0.88, 0.75), ha="center",
                color=colors["market-force"])

    return fig_da


def diseq_stochastic_adjustment_figure():
    fig_sa = plt.figure()
    # with plt.xkcd():
    ax = get_axes(fig_sa)

    q = quantity_space()

    (qeq, peq) = equilibrium(q[50])
    psh = peq * 0.6
    dsh = demanded_quantity(psh)
    ssh = supplied_quantity(psh)

    ax.plot(q, demand(q), color=colors["market-force"])
    ax.plot(q, supply(q), color=colors["market-force"])
    ax.plot(q[q < qeq], demand(q[q < qeq]), "-",
            alpha=0.8, ls="dashdot", color=colors["identification"])
    ax.plot(q[q < qeq], supply(q[q < qeq]), "-",
            alpha=0.8, ls="dashdot", color=colors["identification"])
    ax.plot([dsh, ssh], [psh, psh], color=colors["shortage"])
    ax.plot(dsh, psh, "o", mfc="none", color=colors["shortage"])
    ax.plot(ssh, psh, "o", mfc="none", color=colors["shortage"])

    ax.annotate(
        "$\Delta P$",
        xy=((dsh + ssh) * 0.55, psh * 1.06),
        arrowprops=dict(arrowstyle="->", color=colors["shortage"]),
        xytext=(0.7, 0.5),
        color=colors["shortage"],
    )
    ax.annotate(
        "",
        xy=(0.78, 0.48),
        arrowprops=dict(arrowstyle="<-", color=colors["shortage"]),
        xytext=(0.78, 0.56),
    )
    ax.annotate("$\ +\ u_p\ ?$", xy=(0.78, 0.5),
                xytext=(0.78, 0.5), color=colors["shortage"])

    ax.annotate("Demand", xy=(0.88, 0.22), ha="center",
                color=colors["market-force"])
    ax.annotate("Supply", xy=(0.88, 0.75), ha="center",
                color=colors["market-force"])

    return fig_sa


def invert_image(input_filename, output_filename):
    image = Image.open(input_filename)

    def invert_impl():
        r, g, b, a = image.split()
        rgb_image = Image.merge("RGB", (r, g, b))
        inverted_image = PIL.ImageOps.invert(rgb_image)
        r2, g2, b2 = inverted_image.split()
        final_transparent_image = Image.merge("RGBA", (r2, g2, b2, a))
        final_transparent_image.save(output_filename)

    if image.mode == "RGBA":
        invert_impl()
    else:
        image = image.convert("RGBA")
        data = image.getdata()
        new_data = []
        for item in data:
            if item[0] == 255 and item[1] == 255 and item[2] == 255:
                new_data.append((255, 255, 255, 0))
            else:
                new_data.append(item)
        image.putdata(new_data)
        invert_impl()


def invert_image_in_place(filename):
    invert_image(filename, filename)


def create_figure(model_name):
    output_dir = "../man/figures"

    filename = "{}/{}.png".format(output_dir, model_name)
    eval("{}_figure()".format(model_name))
    plt.savefig(filename, transparent=True)
    invert_image_in_place(filename)

model_names = ["equilibrium", "diseq_basic", "diseq_directional",
               "diseq_deterministic_adjustment", "diseq_stochastic_adjustment"]
for model_name in model_names:
    create_figure(model_name)

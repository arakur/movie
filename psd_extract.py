import json
import sys
import psd_tools
import os
import shutil


# unusable chars for file name
UNUSABLE_CHARS = "#%&{}\\<>*?/ $!'\":@+`|="


class LayerInfo:
    level = 0

    # level: layer level
    def __init__(self, layer: psd_tools.PSDImage):
        self.level = LayerInfo.level
        LayerInfo.level += 1

        self.kind = layer.kind
        self.left = layer.left
        self.top = layer.top

        s = layer.name

        self.radio = False
        self.forced = False
        self.flip = []

        while True:
            if s.startswith("*"):
                s = s[1:]
                self.radio = True
            elif s.startswith("!"):
                s = s[1:]
                self.forced = True
            else:
                break

        while True:
            for f in ["flipx", "flipy", "flipxy"]:
                if s.endswith(":" + f):
                    s = s[: -len(f) - 1]
                    self.flip.append(f)
                    break  # break for, continue while
            else:
                break  # break while

        self.name = s

    def file_name(self) -> str:
        replaced = self.name
        for c in UNUSABLE_CHARS:
            replaced = replaced.replace(c, "_")  # replace unusable chars with "_"
        return str(self.level) + "_" + replaced


def save_layers(layers: psd_tools.PSDImage, path: str, content_dir: str) -> list:
    ret = []
    for layer in layers:
        child = {}
        info = LayerInfo(layer)
        child["kind"] = info.kind
        child["name"] = info.name
        child["level"] = info.level
        child["radio"] = info.radio
        child["forced"] = info.forced
        child["flip"] = info.flip
        child["is_visible"] = layer.is_visible()
        child["left"] = info.left
        child["top"] = info.top

        if layer.is_group():
            child_path = path + "/" + info.file_name()
            os.makedirs(content_dir + "/" + child_path)
            child_dic = save_layers(layer, child_path, content_dir)
            child["children"] = child_dic
            child_path_simp = child_path.replace("./", "")
            child["path"] = child_path_simp
        else:
            layer.topil().save(
                content_dir + "/" + path + "/" + info.file_name() + ".png"
            )
            child["children"] = []
            child_path = path + "/" + info.file_name() + ".png"
            child_path_simp = child_path.replace("./", "")
            child["path"] = child_path_simp
        ret.append(child)
    return ret


# main

if __name__ == "__main__":
    # arguments

    args = sys.argv

    if args == ["-h"] or args == ["--help"]:
        print("Usage: psd_extract.py <path> <out_name> <out_dir>")
        exit(0)

    if len(args) != 4:
        print("Usage: psd_extract.py <path> <out_name> <out_dir>")
        exit(1)

    path = args[1]
    if not os.path.exists(path):
        print("File", path, "not found.")
        exit(1)
    out_name = args[2]
    out_dir = args[3]
    if not os.path.exists(out_dir):
        print("Directory", out_dir, "not found.")
        exit(1)

    # make directory

    content_dir = out_dir + "/" + out_name

    layers = psd_tools.PSDImage.open(path)

    if os.path.exists(content_dir):
        shutil.rmtree(content_dir)
    os.makedirs(content_dir)

    # save layers

    layers_dic = save_layers(layers, ".", content_dir)

    appearance_file = content_dir + "/appearance.json"

    dic = {
        "width": layers.width,
        "height": layers.height,
        "layers": layers_dic,
    }

    json.dump(
        dic,
        open(appearance_file, "w", encoding="utf-8"),
        indent=4,
        ensure_ascii=False,
    )

    print("Done.")
    print("Written to " + content_dir)
    print("Information of layers is at " + appearance_file)

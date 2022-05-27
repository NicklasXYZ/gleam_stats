from lxml import etree
import lxml.html
import os


EXCLUDE_FILES = ["index.html"]


def load_header_conent(content: list[str]) -> str:
    """Loads content from a list of HTML files. 

    Args:
        content: List of HTML files that contain 'script' and 'link' fragments 
            that should be placed into the <head></head> tag of an existing html 
            file.

    Returns:
        HTML 'script' and 'link' string fragments that have been concatenated
        into a single string.  
    """
    s = ""
    for file in content:
        with open(file, "r") as f:
            s += f.read()
        s += "\n"
    return s


def inject_header_content(content: list[str], file_paths: list[str]) -> None:
    """Inject loaded 'link' and 'script' HTML fragments into existing HTML 
    files.

    Args:
        content: List of HTML files that contain 'script' and 'link' fragments 
            that should be placed into the <head></head> tag of an existing html 
            file.
        file_paths: List of existing html file.
    """
    s = load_header_conent(content)
    s = lxml.html.fragments_fromstring(s)
    for file in file_paths:
        with open(file, "r") as f:
            root = lxml.html.fromstring(f.read())
            head = root.find(".//head")
            for fragment in s:
                head.append(fragment)
            ncontent = etree.tostring(
                root,
                encoding="unicode",
                pretty_print=True,
            )
        with open(file, "w+") as f:
            f.write(ncontent)

def traverse_directories(root: str) -> list[str]:
    """Traverse the directory containing HTML pages with documentation which 
    should eventually be modified.

    Args:
        root: Root of directory to traverse.

    Raises:
        FileNotFoundError: If the given 'root' directory does not exist.

    Returns:
        A list of collected paths to HTML files which should be modified.        
    """
    if os.path.exists(root):
        html_file_paths: list[str] = [] 
        for root, dirs, files in os.walk(root):
            for file in files:
                if "html" in file.split("."):
                    if file not in EXCLUDE_FILES:
                        html_file_paths.append(os.path.join(root, file))
        return html_file_paths
    else:
      raise FileNotFoundError(f"{root} does not exist!")


if __name__ == "__main__":
    # Script entrypoint...
    file_paths = traverse_directories(root="../build")
    content = ["katex.html"]
    inject_header_content(content=content, file_paths=file_paths)

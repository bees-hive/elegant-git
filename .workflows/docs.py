"""The script generates 'docs/commands.md' file."""
import subprocess

from typing import List, Sequence


def output(raw) -> str:
    return raw.decode("utf-8")


def command_output(command) -> List[str]:
    return output(subprocess.check_output(command)).splitlines()


def header(command: str):
    return f"# `{command}`\n"


def normalize_command_line(line: str):
    if line.startswith("usage:"):
        return f"```bash\n{line}\n```\n"
    return f"{line}\n"


def body() -> Sequence[str]:
    print("Generate documentation...")
    data = []
    print("Explain 'git-elegant'...")
    data.append(header("git-elegant"))
    data.append("```bash\n")
    data.extend(map(lambda line: f"{line}\n", command_output("bin/git-elegant")))
    data.append("```\n\n")
    for command in command_output(["bin/git-elegant", "show-commands"]):
        print(f"Explain 'git-elegant {command}'...")
        data.append(header(command))
        data.extend(
            map(
                normalize_command_line,
                command_output(["bin/git-elegant", command, "--help"]),
            )
        )
    return data[:-1]


def save_text(documentation: Sequence[str], destination: str) -> None:
    print("Write documentation to", destination)
    with open(destination, "w") as doc:
        doc.writelines(documentation)


if __name__ == "__main__":
    save_text(body(), "docs/commands.md")

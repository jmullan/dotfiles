#!/bin/bash
if [ $# -lt 1 ] ; then
    echo "You have to supply the source directory as an argument"
    exit
fi
DOTFILES_DIR="${1}"
PORTABLE_DIR="${DOTFILES_DIR}/portable"
if [ ! -e "${PORTABLE_DIR}" ] ; then
    echo "Your source directory argument must refer to a source directory"
    exit
fi

function soft_link() {
    FROM=$1
    TO=$2

    TO_DIR="$(dirname "${TO}")"
    # echo "Checking ${FULL_FILE} to ${TO} in ${TO_DIR}"
    if [ -L "${TO}" ] && [ ! -e "${TO}" ]; then
        "${TO}"
    fi
    if [ -e "${FROM}" ] ; then
        if [ ! -e "${TO_DIR}" ] ; then
            mkdir -p "${TO_DIR}"
        fi
        if [ ! -e "${TO}" ] ; then
            echo "Linking ${FULL_FILE} to ${TO}"
            ln -s "${FROM}" "${TO}"
        else
            ls -lAd "${TO}"
        fi
    fi
}

MORE_DIRS=(
    "${DOTFILES_DIR}/portable" "/"
    "${DOTFILES_DIR}/local/$(hostname)" "/"
    "${DOTFILES_DIR}/local/$(hostname)/.config" "/.config/"
    "${DOTFILES_DIR}/.config" "/.config/"
    "${DOTFILES_DIR}/.config/jmullan" "/.config/jmullan/"
)
for ((i = 0; i < ${#MORE_DIRS[@]}; i += 2)); do
    MORE_DIR="${MORE_DIRS[i]}"
    MORE_DIR_TO="${MORE_DIRS[i + 1]}"
    # echo "CHECKING ${MORE_DIR}"
    if [ -e "${MORE_DIR}" ] ; then
        # echo "FINDING IN ${MORE_DIR}"
        for FULL_FILE in $(find "${MORE_DIR}" -mindepth 1 -maxdepth 1 -not -name '.git' | grep -v '~$' | grep -v '.bak') ; do
            # echo "FOUND ${FULL_FILE}"
            RELATIVE_DIR="${FULL_FILE#"${MORE_DIR}/"}"
            # echo "HOME ${HOME}"
            # echo "MORE_DIR_TO ${MORE_DIR_TO}"
            # echo "RELATIVE_DIR ${RELATIVE_DIR}"
            soft_link "${FULL_FILE}" "${HOME}${MORE_DIR_TO}${RELATIVE_DIR}"
        done
    fi
done
exit
LIBRARY="${HOME}/Library/"
if [ -e "${LIBRARY}" ] ; then
    if [ -e "${LIBRARY}/Preferences/PyCharm30/colors" ]; then
        cp "submodules/pycharm-solarized/Solarized Dark.xml" "${LIBRARY}/Preferences/PyCharm30/colors/"
    fi
    mkdir -p "${LIBRARY}/KeyBindings"
    soft_link "$(DOTFILES_DIR)/osx/Library/KeyBindings/DefaultKeyBinding.dict" "${LIBRARY}/KeyBindings/DefaultKeyBinding.dict"

fi
mkdir -p ~/.virtualenvs
mkdir -p ~/.pyenv
if [ ! -e ~/.pyenv/.git ] ; then
    cd ~/.pyenv/
    git init
    git remote add origin https://github.com/pyenv/pyenv.git
    git fetch
    git checkuot master
    git status
fi

#!/usr/bin/env bash
set -e

indent() {
    sed 's/^/    /'
}

PKGNAME_DEFAULT="$(basename "$(pwd)")"
if [ "$PKGNAME_DEFAULT" = "template-haskell" ]; then
    PKGNAME_DEFAULT="my-project"
fi
read -r -p "Package name [$PKGNAME_DEFAULT]: " PKGNAME
PKGNAME=${PKGNAME:-$PKGNAME_DEFAULT}

AUTHNAME_DEFAULT=$(git config --default "Firstname Lastname" --get user.name)
read -r -p "Author name [$AUTHNAME_DEFAULT]: " AUTHNAME
AUTHNAME=${AUTHNAME:-$AUTHNAME_DEFAULT}

EMAIL_DEFAULT=$(git config --default "user@email.com" --get user.email)
read -r -p "Author email [$EMAIL_DEFAULT]: " EMAIL
EMAIL=${EMAIL:-$EMAIL_DEFAULT}

currentyear=$(date +"%Y")

echo "Running template-haskell Haskell project generator wizard"

echo "Substituting placeholder variables..."
(
    set -x
    find . -type f -not -name 'wizard.sh' -exec sed \
        -e "s#PKGNAME#$PKGNAME#g" \
        -e "s#AUTHNAME#$AUTHNAME#g" \
        -e "s#EMAIL#$EMAIL#g" \
        -e "s#CURRENTYEAR#$currentyear#g" \
        -i '' '{}' \;
) 2>&1 | indent

echo "Renaming files..."
(
    set -x
    mv "PKGNAME.cabal" "$PKGNAME.cabal"
    mv "README_TEMPLATE.md" "README.md"
) 2>&1 | indent

echo "Cleaning up..."
(
    set -x
    rm wizard.sh
) 2>&1 | indent

echo "All set!"

#!/usr/bin/env bash
# Indicador global de agente — mostra animação Knight Rider em qualquer janela
# enquanto houver um agente rodando em qualquer pane da sessão.

if ! command -v tmux >/dev/null 2>&1; then exit 0; fi

# Checa se algum pane em qualquer janela tem agente rodando
if ! tmux show-environment -g 2>/dev/null | grep -q '_STATE=running'; then
    printf ''
    exit 0
fi

# Pega o frame atual da animação (atualizado pelo animation.sh via refresh-client -S)
frame=$(tmux show-environment -g TMUX_AGENT_ANIMATION_FRAME 2>/dev/null \
    | sed 's/TMUX_AGENT_ANIMATION_FRAME=//')

if [ -z "$frame" ]; then
    # Animação não iniciada ainda — mostra barra estática
    printf '#[fg=colour160]━━━━━━━#[default]'
    exit 0
fi

# Renderiza Knight Rider (mesma lógica do plugin)
bar_width=7
bright="colour196"
trail="colour160"
dim="colour52"
seg=""

for (( i = 0; i < bar_width; i++ )); do
    if [ "$i" -eq "$frame" ]; then
        color="$bright"
    elif [ "$i" -eq $((frame - 1)) ] || [ "$i" -eq $((frame + 1)) ]; then
        color="$trail"
    else
        color="$dim"
    fi
    seg="${seg}#[fg=${color}]━"
done

printf '%s#[default]' "$seg"

[![MELPA](http://melpa.org/packages/helm-mt-badge.svg)](http://melpa.org/#/helm-mt)
[![MELPA Stable](http://stable.melpa.org/packages/helm-mt-badge.svg)](http://stable.melpa.org/#/helm-mt)

# helm-mt
Helm bindings for managing [`multi-term`](https://www.emacswiki.org/emacs/MultiTerm) terminals as well as shells.

A call to `helm-mt` will show a list of terminal sessions managed by `multi-term` as well as buffers with major mode `shell-mode`.

From there, you are able to create, delete or switch over to existing terminal buffers.

![helm-mt](mt.gif)

# Setup
Invoke `helm-mt` and bind it to a keyboard shortcut:

```elisp
(require 'helm-mt)
(global-set-key (kbd "C-x t") 'helm-mt)
```

If you would like to run `helm-mt` when you do `M-x term` or `M-x shell`, then put this in your init file:

```elisp
(helm-mt/reroute-terminal-functions t)
```

To deactivate this behavior again, pass `nil` instead of `t`.

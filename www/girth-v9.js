/* Progressive interaction layer for the v9 shell.
   Everything here degrades safely: Shiny owns state; this file adds keyboard,
   announcement, filtering, and sharing affordances only. */
(function () {
  'use strict';

  function ready(fn) {
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', fn, { once: true });
    } else {
      fn();
    }
  }

  function setBarState(bar) {
    if (!bar) return;
    var trigger = bar.querySelector('.cb-head');
    var body = bar.querySelector('.cb-body');
    var open = !bar.classList.contains('collapsed');
    if (trigger) trigger.setAttribute('aria-expanded', open ? 'true' : 'false');
    if (body) body.setAttribute('aria-hidden', open ? 'false' : 'true');
  }

  function initControlBar() {
    var bar = document.querySelector('.control-bar');
    if (!bar || bar.dataset.v9Ready === '1') return;
    bar.dataset.v9Ready = '1';
    setBarState(bar);
    new MutationObserver(function () { setBarState(bar); }).observe(bar, {
      attributes: true,
      attributeFilter: ['class']
    });
  }

  function setProgramBrowserState(box) {
    if (!box) return;
    var trigger = box.querySelector('[data-widget="collapse"]');
    var body = box.querySelector('.box-body');
    var open = !box.classList.contains('collapsed-box');
    if (body && !body.id) body.id = 'home_program_browser_panel';
    if (!trigger) return;
    trigger.setAttribute('aria-expanded', open ? 'true' : 'false');
    trigger.setAttribute(
      'aria-label',
      open ? 'Collapse program browser' : 'Expand program browser'
    );
    if (body && body.id) trigger.setAttribute('aria-controls', body.id);
  }

  function initProgramBrowserA11y(root) {
    (root || document).querySelectorAll('.gi-team-browser .box').forEach(
      function (box) { setProgramBrowserState(box); }
    );
  }

  function toast(message) {
    if (typeof window.giToast === 'function') {
      var t = window.giToast(message);
      return function (done) { t.done(done); };
    }
    return function () {};
  }

  function copyShareLink(button) {
    var done = toast('Copying this exact view…');
    var url = window.location.href;
    var copied = function () {
      done('Link copied — team, filters and tab included');
      if (button) {
        button.setAttribute('aria-label', 'Link copied');
        window.setTimeout(function () {
          button.setAttribute('aria-label', 'Copy a link to this view');
        }, 1800);
      }
    };
    var failed = function () { done('Could not copy — use the browser address bar'); };

    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(url).then(copied).catch(failed);
      return;
    }
    var field = document.createElement('textarea');
    field.value = url;
    field.setAttribute('readonly', '');
    field.style.position = 'fixed';
    field.style.opacity = '0';
    document.body.appendChild(field);
    field.select();
    try {
      if (document.execCommand('copy')) {
        copied();
      } else {
        failed();
      }
    } catch (err) {
      failed();
    }
    field.remove();
  }

  function filterTeamModal(input) {
    var modal = input.closest('.modal-content') || document;
    var query = input.value.trim().toLowerCase();
    var visible = 0;
    modal.querySelectorAll('.gi-modal-team').forEach(function (button) {
      var name = (button.getAttribute('data-team') || button.textContent || '').toLowerCase();
      var show = !query || name.indexOf(query) !== -1;
      button.hidden = !show;
      if (show) visible += 1;
    });
    modal.querySelectorAll('.gi-modal-section').forEach(function (section) {
      section.hidden = section.querySelectorAll('.gi-modal-team:not([hidden])').length === 0;
    });
    var empty = modal.querySelector('.gi-team-empty');
    if (empty) {
      empty.style.display = visible ? 'none' : 'block';
      empty.textContent = visible ? '' : 'No Power-4 program matches “' + input.value.trim() + '”.';
    }
    input.setAttribute('aria-label', visible + ' matching programs. Search teams');
  }

  function filterHomeTeams(input) {
    var browser = input.closest('.gi-team-browser') || document;
    var query = input.value.trim().toLowerCase();
    var visible = 0;
    browser.querySelectorAll('.gi-home-team').forEach(function (button) {
      var name = (button.textContent || '').trim().toLowerCase();
      var show = !query || name.indexOf(query) !== -1;
      button.hidden = !show;
      if (show) visible += 1;
    });
    browser.querySelectorAll('.gi-team-grid').forEach(function (grid) {
      var hasVisible = grid.querySelector('.gi-home-team:not([hidden])') !== null;
      grid.hidden = !hasVisible;
      var heading = grid.previousElementSibling;
      if (heading && heading.classList.contains('gi-pick-conf')) {
        heading.hidden = !hasVisible;
      }
    });
    var empty = browser.querySelector('.gi-home-empty');
    if (!empty) {
      empty = document.createElement('p');
      empty.className = 'gi-team-empty gi-home-empty';
      empty.setAttribute('role', 'status');
      empty.setAttribute('aria-live', 'polite');
      input.insertAdjacentElement('afterend', empty);
    }
    empty.style.display = visible ? 'none' : 'block';
    empty.textContent = visible ? '' : 'No Power-4 program matches “' + input.value.trim() + '”.';
    input.setAttribute('aria-label', visible + ' matching programs. Search all programs');
  }

  function enhanceDynamicUi(root) {
    (root || document).querySelectorAll('.vb-link:not([data-a11y-ready])').forEach(function (card) {
      card.dataset.a11yReady = '1';
      if (card.tagName === 'BUTTON' ||
          (card.tagName === 'A' && card.hasAttribute('href'))) return;
      card.setAttribute('role', 'button');
      card.setAttribute('tabindex', '0');
      card.setAttribute('aria-label', card.getAttribute('title') || 'Open detail view');
      card.addEventListener('keydown', function (event) {
        if (event.key !== 'Enter' && event.key !== ' ') return;
        event.preventDefault();
        card.click();
      });
    });

    (root || document).querySelectorAll('a[target="_blank"]:not([rel])').forEach(function (link) {
      link.setAttribute('rel', 'noopener noreferrer');
    });
  }

  function updateActiveNavigation() {
    document.querySelectorAll('.sidebar-menu a[aria-current]').forEach(function (link) {
      link.removeAttribute('aria-current');
    });
    var active = document.querySelector('.sidebar-menu li.active > a');
    if (!active) return;
    active.setAttribute('aria-current', 'page');
    var label = (active.textContent || '').trim().replace(/\s+/g, ' ');
    if (label) document.title = label + ' · Power-4 Girth Index';
  }

  ready(function () {
    initControlBar();
    enhanceDynamicUi(document);
    initProgramBrowserA11y(document);
    updateActiveNavigation();

    if (window.jQuery) {
      window.jQuery(document).on(
        'shown.bs.tab',
        '.sidebar-menu a[href^="#shiny-tab-"]',
        function () {
          updateActiveNavigation();
          if (window.matchMedia('(max-width: 767px)').matches) {
            window.scrollTo({ top: 0, behavior: 'auto' });
          }
        });
      window.jQuery(document).on(
        'expanded.boxwidget collapsed.boxwidget',
        '.gi-team-browser .box',
        function () {
          setProgramBrowserState(this);
        }
      );
    }

    document.addEventListener('click', function (event) {
      var share = event.target.closest('#copy_view_link');
      if (share) {
        event.preventDefault();
        event.stopPropagation();
        copyShareLink(share);
        return;
      }

      var nav = event.target.closest('.sidebar-menu a[href^="#shiny-tab-"]');
      if (nav) {
        window.setTimeout(function () {
          updateActiveNavigation();
          if (window.matchMedia('(max-width: 767px)').matches) {
            window.scrollTo({ top: 0, behavior: 'auto' });
          }
        }, 30);
      }
    });

    document.addEventListener('input', function (event) {
      if (!event.target) return;
      if (event.target.id === 'team_modal_search') {
        filterTeamModal(event.target);
      } else if (event.target.id === 'home_team_search') {
        filterHomeTeams(event.target);
      }
    });

    var observerQueued = false;
    var observer = new MutationObserver(function (mutations) {
      var relevant = mutations.some(function (mutation) {
        return Array.prototype.some.call(mutation.addedNodes, function (node) {
          if (node.nodeType !== 1) return false;
          if (node.matches('.vb-link, a[target="_blank"], .control-bar, .gi-team-browser')) {
            return true;
          }
          return node.querySelector(
            '.vb-link, a[target="_blank"], .control-bar, .gi-team-browser'
          ) !== null;
        });
      });
      if (!relevant || observerQueued) return;
      observerQueued = true;
      window.requestAnimationFrame(function () {
        observerQueued = false;
        enhanceDynamicUi(document);
        initControlBar();
        initProgramBrowserA11y(document);
        updateActiveNavigation();
      });
    });
    observer.observe(document.body, { childList: true, subtree: true });
  });
})();

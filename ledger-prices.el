;;; ledger-prices.el --- Fetch stock, currency and cryptocurrency prices for ledger -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://www.github.com/benthamite/ledger-prices
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Fetch stock, currency and cryptocurrency prices for `ledger'.

;;; Code:

(require 'request)
(require 'json)

;;;; User options

(defgroup ledger-prices ()
  "Get stock, currency and cryptocurrency prices for ledger."
  :group 'files
  :link '(url-link :tag "Homepage" "https://github.com/benthamite/ledger-prices"))

(defcustom ledger-prices-api-key-finnhub ""
  "Your `finnhub.io' API key."
  :group 'ledger-prices
  :type 'string)

(defcustom ledger-prices-api-key-exchangerates ""
  "Your `exchangeratesapi.io' API key."
  :group 'ledger-prices
  :type 'string)

(defcustom ledger-prices-pricedb-file ""
  "Path to your `.pricedb' file."
  :group 'ledger-prices
  :type 'file)

(defcustom ledger-prices-currency "USD"
  "Currency for fetching prices."
  :group 'ledger-prices
  :type 'string)

(defcustom ledger-prices-separator "\n"
  "Preface new entries added to `.pricedb' with this string."
  :group 'ledger-prices
  :type 'string)

(defcustom ledger-prices-stocks '()
  "List of stocks to get prices for."
  :group 'ledger-prices
  :type 'list)

(defcustom ledger-prices-currencies '()
  "List of currencies to get prices for."
  :group 'ledger-prices
  :type 'list)

;;;; <individual section continue>

(defun ledger-prices-fetch-stocks (symbol)
  "Fetch price for stock with SYMBOL."
  (let ((response (request-response-data
		   (request
		     (concat "https://finnhub.io/api/v1/quote?symbol="
			     symbol "&token=" ledger-prices-api-key-finnhub)
		     :sync t
		     :parser 'json-read))))
    (if (assoc-default 'error response)
        (user-error "Failed to fetch price for %s: %s" symbol (assoc-default 'error response))
      (when-let* ((price-entry (assoc-default 'c response)))
        (number-to-string price-entry)))))

(defun ledger-prices-fetch-currency (currency &optional denomination)
  "Fetch price for CURRENCY from the `Exchangerates' API.
If no DENOMINATION is provided, user the currency specified in `ledger-prices-currency'."
  (let ((denomination (intern (or denomination ledger-prices-currency)))
	(response (request-response-data
		   (request
		     (concat "http://api.exchangeratesapi.io/latest?base=" currency
			     "&access_key=" ledger-prices-api-key-exchangerates)
		     :sync t
		     :parser 'json-read))))
    (if (assoc-default 'error response)
	(user-error "Failed to fetch rate for %s: %s" currency (assoc-default 'error response))
      (when-let* ((rates (assoc-default 'rates response)))
	(alist-get denomination rates)))))

(defun ledger-prices-fetch-multiple (symbols type)
  "Fetch price for symbols in list SYMBOLS of TYPE.
Return an alist of the form ((SYMBOL . PRICE) ...)."
  ;; iterate over `ledger-prices-fetch-stocks'
  (let ((prices '()))
    (dolist (symbol symbols)
      (when-let ((price (funcall  symbol type)))
	(push (cons symbol price) prices)))
    prices))

(defun ledger-prices-fetch-type (symbol type)
  "Fetch price for SYMBOL of TYPE."
  (pcase type
    ('stocks (ledger-prices-fetch-stocks symbol))
    ('currencies (ledger-prices-fetch-currencies symbol))
    (_ (user-error "Unknown type: %s" type))))

(defun ledger-prices-make-entries (symbols type)
  "Return entries in `.pricedb' ledger format for SYMBOLS of TYPE."
  (let ((alist (ledger-prices-fetch-multiple symbols type)))
    (mapconcat (lambda (cons)
		 (format "P %s %s %s USD"
			 (format-time-string "%Y-%m-%d")
			 (car cons) (cdr cons)))
	       alist "\n")))

(defun ledger-prices-write-file (entries)
  "Write the ENTRIES to the `.pricedb' file."
  (with-current-buffer (find-file-noselect ledger-prices-pricedb-file)
    (goto-char (point-max))
    (insert ledger-prices-separator)
    (insert entries)
    (save-buffer)))

;;;###autoload
(defun ledger-prices-update-stocks ()
  "Update stock prices.
Fetch prices for each symbol in `ledger-prices-stocks' and write the `.pricedb'
file as defined in `ledger-prices-pricedb-file'."
  (interactive)
  (let ((entries (ledger-prices-make-entries ledger-prices-stocks 'stocks)))
    (ledger-prices-write-file entries)))

(defun ledger-prices-update-currencies ()
  "Update currency prices.
Fetch prices for each symbol in `ledger-prices-currencies' and write the
`.pricedb' file as defined in `ledger-prices-pricedb-file'."
  (interactive)
  (let ((entries (ledger-prices-make-entries ledger-prices-currencies 'currencies)))
    (ledger-prices-write-file entries)))

(provide 'ledger-prices)

;;; ledger-prices.el ends here

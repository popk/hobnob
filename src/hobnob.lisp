;;;; hobnob.lisp

(in-package #:hobnob)

;;; "hobnob" goes here. Hacks and glory await!

;;; Default port on which the webserver listens
(defconstant +port+ 3000)
(defvar *server* nil)


;;
;; Hobnob Exported Functions
;;

(defun start-server (&key (port +port+))
  "Start the Hobnob server. This primarily starts the hunchetoot webserver,
and serves pages that are needed for the hobnob application.

port - The HTTP port on which hobnob server needs to listen. By default
       this is 3000.
"
  (setf (cl-who:html-mode) :html5)
  (setf *server* (hunchentoot:start 
		  (make-instance 'hunchentoot:easy-acceptor
				 :port port))))

(defun stop-server ()
  "Stop the Hobnob server. The webserver is brought down and the application
is no longer running."
  (hunchentoot:stop *server*))


;;
;; Hunchentoot Route Handlers
;;

(hunchentoot:define-easy-handler (index :uri "/") ()
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
	   (html-head "Internet of Things Simulator | Hobnob")
	   (:body
	    (html-navbar)
	    (:div :class "container"
		  (:div :class "col-md-8"
			(:h1 "Introducing Hobnob")
			(:p "Hobnob is an Internet of Things protocol simulator. It can be used to test
different scenarios. The goal of hobnob is to support the following protocols")
			(:ol
			 (:li "CoAP Protocol.")
			 (:li "MQTT Protocol."))
			(:p "The current implementation of hobnob supports only parts of CoAP. See "
			    (:a :href "/features" "features") " for more details.")
			(html-js-scripts)))))))

(hunchentoot:define-easy-handler (about :uri "/about") ()
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
	   (html-head "About | Hobnob")
	   (:body
	    (html-navbar)
	    (:div :class "container"
		  (:div :class "col-md-8" 
			(:h1 "About Hobnob")
			(:p "Hobnob is an Internet of Things Protocol Simulator by " (:a :href "http://www.heptaxel.com" "Sid Heroor") ".")))
	    (html-js-scripts)))))

(hunchentoot:define-easy-handler (features :uri "/features") ()
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
	   (html-head "Features | Hobnob")
	   (:body
	    (html-navbar)
	    (:div :class "container"
		  (:div :class "col-md-8" 
			(:h1 "Supported Features")
			(:ul
			 (:li "CoAP Protocol packet format"))
			(:h1 "TODO")
			(:ul
			 (:li "MQTT Protocol"))))
	    (html-js-scripts)))))

(hunchentoot:define-easy-handler (css-scripts :uri "/styles.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (get-css))


;;
;; HTML Helper functions and macros for cl-who
;; These functions are use to wrap boilerplate bootstrap html
;;


(defmacro html-comment (c)
  "cl-who does not have a mechanism to generate HTML comments.
Implement a macro to insert HTML comments <!-- -->."
  `(cl-who:str (format t "~%<!-- ~A -->" ,c)))

(defun html-head (the-title)
  "Insert a Hobnob specific HTML <head>. We use Bootstrap for
responsiveness. Link to the same via CDN."
  (cl-who:with-html-output (*standard-output* nil :indent t)
      (:head
       (:meta :charset "utf-8")
       (:meta :http-equiv "X-UA-Compatible" :content "IE=edge")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:title (cl-who:str the-title))
       (html-comment "HTML5 Shim and Respond.js IE8 support of HTML5 elements and media queries")
       (html-comment "[if lt IE 9]>
<script src='https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js'></script>
<script src='https://oss.maxcdn.com/respond/1.4.2/respond.min.js'></script>
<![endif]")
       (:link 
	:rel "stylesheet"
	:href "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
       (:link
	:rel "stylesheet"
	:href "/styles.css"))))

(defun html-navbar ()
  "Generate a Bootstrap Navbar for Hobnob.
"
  (cl-who:with-html-output (*standard-output* nil :indent t)
    (:div :class "navbar navbar-inverse navbar-fixed-top" :role "navigation"
	  (:div :class "container"
		(:div :class "navbar-header"
		      (:button
		       :type "button"
		       :class "navbar-toggle collapsed"
		       :data-toggle "collapse"
		       :data-target ".navbar-collapse"
		       (:span :class "sr-only" "Toggle Navigation")
		       (:span :class "icon-bar")
		       (:span :class "icon-bar")
		       (:span :class "icon-bar"))
		      (:a :class "navbar-brand" :href "/" "Hobnob"))
		(:div :class "collapse navbar-collapse"
		      (:ul :class "nav navbar-nav"
			   (:li (:a :href "/" "Home"))
			   (:li (:a :href "/about" "About"))
			   (:li (:a :href "/features" "Features"))))))))


(defun html-js-scripts ()
  "Include JQuery and Bootstrap JS libraries"
  (cl-who:with-html-output (*standard-output* nil :indent t)
    (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js")
    (:script
     :src "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js")))

(defun get-css ()
  "Generate the inline CSS."
  (css-lite:css 
    (("body") (:margin "40px 0 0 0"))))

;;;
;;; Other Helper functions
;;;

(defun resource-path (relative-path)
  "Look up a path relative to this ASDF system.

credit - http://ryepup.unwashedmeme.com/blog/2011/06/18/getting-started-with-hunchentoot-and-talcl-websites/"
  (truename (asdf:system-relative-pathname :hobnob relative-path)))

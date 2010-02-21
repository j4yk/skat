(in-package gl-ui)

(defclass agar-test (module)
  ((wins :accessor wins :initform nil)))

(defmethod initialize-instance :after ((mod agar-test) &key)
  (dotimes (n 5) (let ((w (ag:window-new))) (push w (wins mod)) (ag:window-show w))))

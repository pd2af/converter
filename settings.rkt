#lang racket

(provide (all-defined-out))

(define settings
	(hash
		'root-url "http://pd2af.me"
		'paths
			(hash
				'local-win-ab-24 "../../../../../../../../denis_core/projects/sbgn-pd2af/server/static/generated"
				'pd2af.me "/server/pd2af/static/generated"
				'pd2af.org "/home/jpellet/pd2af/server/static/generated"
			)
	)
)

(define (get-settings . path)
	(letrec ((get-settings-rec
								(λ (settings pathlst)
										(cond
											((empty? pathlst) settings)
											(else (get-settings-rec (hash-ref settings (car pathlst)) (cdr pathlst)))))))
		(get-settings-rec settings path)))

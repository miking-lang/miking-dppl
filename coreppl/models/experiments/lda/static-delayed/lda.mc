let model = lam.
	let alpha:[Float] = create numtopics (lam. 1.) in
	let beta:[Float] = create vocabsize (lam. 1.) in
	let theta = create numdocs (lam. assume (Dirichlet alpha)) in
	let phi = create numtopics (lam. assume (Dirichlet beta))  in
	iter (lam doc.
	    let theta = get theta (doc.0) in
	    iter (lam w.
	      let z = assume (Categorical theta) in
	      let phi = get phi z in
	      observe w (Categorical phi)
	  ) doc.1
	  ) docs


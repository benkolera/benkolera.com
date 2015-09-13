---
title: Why I love Haskell
type: post
tags: haskell,functional programming
---

Lorem ipsum dolor sit amet, ius te modus sapientem persequeris, meliore sapientem reprehendunt an eos. Movet consul eu quo, per ad putent oporteat disputationi. Euripidis evertitur intellegebat ne usu. Eirmod persecuti his et, nibh maiorum habemus et vel. Per nisl nobis veniam ut, per ut saepe deserunt. Sumo ignota consectetuer ad mei, ne docendi lucilius sadipscing eum.

```haskell
create ["atom.xml"] $ do
    route idRoute
    compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots "posts/*" "content"
        renderAtom myFeedConfiguration feedCtx posts
```

No malorum adipisci est, eos doming definitionem ex. Sed eu nisl omnis iisque, in vel posse reprehendunt. Odio nulla aliquam te sea, at eum laudem recusabo. Nec in nulla recteque.

Ut qui adhuc voluptaria. Pri fuisset maiestatis constituam ex, rebum reprehendunt ad vim. Propriae officiis praesent ut per, suas primis ceteros usu ne. Agam eloquentiam cu ius. Amet assueverit sea ne, vim adhuc autem dolore ad. Duo eu elitr accumsan copiosae, ea vel eligendi corrumpit, ut pro salutandi reprimique theophrastus.ming Soon!

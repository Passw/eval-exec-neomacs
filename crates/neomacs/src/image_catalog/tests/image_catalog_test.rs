use super::*;
use neomacs_display_runtime::render_thread::ImageRenderState;
use neovm_core::emacs_core::Context;
use neovm_core::emacs_core::Value;
use neovm_core::emacs_core::image_catalog::{
    AxisSize, ImageColorContext, ImageDefaultScale, ImageScaleEnvironment, ImageScalePolicy,
    ImageSizeSpec, ImageSpecIdentity,
};
use std::sync::Arc;
use std::time::Instant;

thread_local! {
    static IMAGE_SPEC_TEST_CONTEXT: Context = Context::new();
}

fn file_request(path: &str) -> ImageResolveRequest {
    let spec = IMAGE_SPEC_TEST_CONTEXT.with(|_| {
        Value::list(vec![
            Value::symbol("image"),
            Value::keyword(":type"),
            Value::symbol("png"),
            Value::keyword(":file"),
            Value::string(path),
        ])
    });
    ImageResolveRequest {
        spec: ImageSpecIdentity::from_lisp_spec(&spec).expect("test image spec"),
        source: ImageResolveSource::File(LispString::from_utf8(path)),
        size: ImageSizeSpec::new(AxisSize::AtMost(24), AxisSize::AtMost(24)),
        rotation: Default::default(),
        colors: ImageColorContext::default(),
        mask: Default::default(),
        frame: Default::default(),
        realization: Default::default(),
    }
}

fn classify(file: &str) -> (ImageResolveRequest, Option<ImageFileRequest>) {
    let (cmd_tx, _cmd_rx) = crossbeam_channel::unbounded();
    let metadata = Arc::new(ImageRenderState::default());
    let catalog = AsyncImageCatalog::new(cmd_tx, None, metadata, None);
    catalog.classify_request(file_request(file))
}

#[test]
fn relative_file_is_classified_for_off_thread_search() {
    // The #242 fix: a bare relative `:file` must be searched against
    // data-directory/images off-thread, not opened verbatim from the cwd.
    let (request, resolution) = classify("splash.svg");
    assert!(matches!(
        &request.source,
        ImageResolveSource::File(p) if p.as_utf8_str() == Some("splash.svg")
    ));
    let resolution = resolution.expect("file source is classified");
    assert!(matches!(resolution, ImageFileRequest::Search { .. }));
    assert!(resolution.needs_off_thread());
}

#[test]
fn absolute_file_is_resolved_inline_and_keys_on_itself() {
    let (request, resolution) = classify("/abs/icon.png");
    assert!(matches!(
        &request.source,
        ImageResolveSource::File(p) if p.as_utf8_str() == Some("/abs/icon.png")
    ));
    let resolution = resolution.expect("file source is classified");
    assert!(matches!(resolution, ImageFileRequest::Direct(_)));
    assert!(!resolution.needs_off_thread());
}

#[test]
fn named_user_file_is_deferred_off_thread() {
    // `~user` may consult NSS/LDAP; keep resolution off the evaluator thread.
    let (request, resolution) = classify("~some-user/x.png");
    assert!(matches!(
        &request.source,
        ImageResolveSource::File(p) if p.as_utf8_str() == Some("~some-user/x.png")
    ));
    let resolution = resolution.expect("file source is classified");
    assert!(matches!(resolution, ImageFileRequest::ExpandHome(_)));
    assert!(resolution.needs_off_thread());
}

#[test]
fn pending_slot_and_decode_command_share_one_resolved_realization() {
    let (cmd_tx, cmd_rx) = crossbeam_channel::unbounded();
    let metadata = Arc::new(ImageRenderState::default());
    let catalog = AsyncImageCatalog::new(cmd_tx, None, metadata, None);
    let mut request = file_request("/tmp/icon.svg");
    // Neither axis pinned: the placeholder falls back to the realization.
    request.size = ImageSizeSpec::new(AxisSize::Native, AxisSize::AtMost(24));
    request.realization = ImageScaleEnvironment::new(7.2, 1.75, ImageDefaultScale::Auto)
        .resolve(ImageScalePolicy::Default);

    let placement = catalog.lookup(request).placement();

    assert_eq!(placement.width(), 18);
    assert_eq!(placement.height(), 18);
    assert!(matches!(
        cmd_rx.try_recv().expect("image load command"),
        RenderCommand::Asset(AssetCommand::ImageLoadFile {
            realization,
            ..
        }) if (realization.layout_scale() - (1.3 / 1.75)).abs() < 0.0001
            && (realization.device_scale() - 1.75).abs() < f32::EPSILON
    ));
}

#[test]
fn invalidate_all_requeues_every_entry_under_its_existing_id() {
    let (cmd_tx, cmd_rx) = crossbeam_channel::unbounded();
    let metadata = Arc::new(ImageRenderState::default());
    let catalog = AsyncImageCatalog::new(cmd_tx, None, metadata, None);

    let first = catalog
        .lookup(file_request("/tmp/one.png"))
        .placement()
        .image_id();
    let second = catalog
        .lookup(file_request("/tmp/two.png"))
        .placement()
        .image_id();
    // Drain the two initial load commands.
    assert!(cmd_rx.try_recv().is_ok());
    assert!(cmd_rx.try_recv().is_ok());

    catalog.invalidate_all();

    let mut requeued_ids = Vec::new();
    while let Ok(command) = cmd_rx.try_recv() {
        match command {
            RenderCommand::Asset(AssetCommand::ImageLoadFile { load, .. }) => {
                requeued_ids.push(load.image());
            }
            other => panic!("unexpected command re-queued: {other:?}"),
        }
    }
    requeued_ids.sort_unstable();
    let mut expected = vec![first, second];
    expected.sort_unstable();
    assert_eq!(requeued_ids, expected, "same ids, one command per entry");

    // The entries survive: a later lookup reuses the id, no new load.
    let again = catalog
        .lookup(file_request("/tmp/one.png"))
        .placement()
        .image_id();
    assert_eq!(again, first);
    assert!(cmd_rx.try_recv().is_err());
}

#[test]
fn invalidating_dependency_retires_old_identity_and_next_lookup_reloads() {
    let (cmd_tx, cmd_rx) = crossbeam_channel::unbounded();
    let metadata = Arc::new(ImageRenderState::default());
    let catalog = AsyncImageCatalog::new(cmd_tx, None, metadata, None);
    let request = file_request("/tmp/watched.svg");

    let first = catalog.lookup(request.clone()).placement().image_id();
    assert!(matches!(
        cmd_rx.try_recv().expect("initial image load"),
        RenderCommand::Asset(AssetCommand::ImageLoadFile { load, .. })
            if load.image() == first
    ));

    catalog.invalidate(ImageInvalidation::Dependency(request.source.clone()));
    assert!(matches!(
        cmd_rx.try_recv().expect("old image identity retired"),
        RenderCommand::Asset(AssetCommand::ImageRetire { image }) if image == first
    ));

    let second = catalog.lookup(request).placement().image_id();
    assert_ne!(first, second);
    assert!(matches!(
        cmd_rx.try_recv().expect("replacement image load"),
        RenderCommand::Asset(AssetCommand::ImageLoadFile { load, .. })
            if load.image() == second
    ));
}

#[test]
fn invalidating_spec_preserves_other_spec_that_uses_same_dependency() {
    let (cmd_tx, cmd_rx) = crossbeam_channel::unbounded();
    let metadata = Arc::new(ImageRenderState::default());
    let catalog = AsyncImageCatalog::new(cmd_tx, None, metadata, None);
    let first = file_request("/tmp/multi-page.png");
    let mut second = first.clone();
    let second_spec = Value::list(vec![
        Value::symbol("image"),
        Value::keyword(":type"),
        Value::symbol("png"),
        Value::keyword(":file"),
        Value::string("/tmp/multi-page.png"),
        Value::keyword(":index"),
        Value::fixnum(1),
    ]);
    second.spec = ImageSpecIdentity::from_lisp_spec(&second_spec).expect("second test image spec");

    let first_id = catalog.lookup(first.clone()).placement().image_id();
    let second_id = catalog.lookup(second.clone()).placement().image_id();
    assert_ne!(first_id, second_id);
    assert!(cmd_rx.try_recv().is_ok());
    assert!(cmd_rx.try_recv().is_ok());

    catalog.invalidate(ImageInvalidation::Spec {
        spec: first.spec.clone(),
    });
    assert!(matches!(
        cmd_rx.try_recv().expect("only exact spec identity freed"),
        RenderCommand::Asset(AssetCommand::ImageRetire { image }) if image == first_id
    ));
    assert!(cmd_rx.try_recv().is_err());

    assert_eq!(
        catalog.lookup(second).placement().image_id(),
        second_id,
        "the other spec keeps its renderer identity"
    );
    assert!(cmd_rx.try_recv().is_err());

    let replacement_id = catalog.lookup(first).placement().image_id();
    assert_ne!(replacement_id, first_id);
    assert!(matches!(
        cmd_rx.try_recv().expect("exact spec is decoded again"),
        RenderCommand::Asset(AssetCommand::ImageLoadFile { load, .. })
            if load.image() == replacement_id
    ));
}

#[test]
fn renderer_reconciliation_upgrades_pending_to_ready_geometry() {
    use neomacs_display_runtime::render_thread::ImageDecodeTerminal;
    use neovm_core::emacs_core::image_catalog::{ImageLookup, ResolvedImageMetadata};

    let (cmd_tx, _cmd_rx) = crossbeam_channel::unbounded();
    let metadata = Arc::new(ImageRenderState::default());
    let catalog = AsyncImageCatalog::new(cmd_tx, None, Arc::clone(&metadata), None);
    let request = file_request("/tmp/promote.png");

    let ImageLookup::Pending(pending) = catalog.lookup(request.clone()) else {
        panic!("expected pending");
    };
    let id = pending.placement().image_id();
    let load = pending.load();
    // Placeholder from AtMost(24) pins.
    assert_eq!(pending.placement().width(), 24);

    metadata.publish_terminal(
        load,
        ImageDecodeTerminal::Ready(ResolvedImageMetadata::layout_is_image_pixels(
            120,
            80,
            0,
            false,
            Default::default(),
        )),
    );

    catalog.reconcile_renderer_state(ImageStateEvent::DecodeCompleted(load));
    let ImageLookup::Ready(ready) = catalog.lookup(request) else {
        panic!("promote must leave Ready geometry for rebuild");
    };
    assert_eq!(ready.metadata.layout.dimensions(), (120, 80));
    assert_eq!(ready.image_id(), id);
}

#[test]
fn renderer_eviction_requeues_ready_image_under_its_stable_id() {
    use neomacs_display_runtime::render_thread::ImageDecodeTerminal;
    use neovm_core::emacs_core::image_catalog::{ImageLookup, ResolvedImageMetadata};

    let (cmd_tx, cmd_rx) = crossbeam_channel::unbounded();
    let metadata = Arc::new(ImageRenderState::default());
    let catalog = AsyncImageCatalog::new(cmd_tx, None, Arc::clone(&metadata), None);
    let request = file_request("/tmp/room-avatar.png");

    let ImageLookup::Pending(pending) = catalog.lookup(request.clone()) else {
        panic!("new avatar should begin pending");
    };
    let id = pending.placement().image_id();
    let first_load = pending.load();
    assert!(matches!(
        cmd_rx.try_recv().expect("initial avatar load"),
        RenderCommand::Asset(AssetCommand::ImageLoadFile { load, .. })
            if load == first_load
    ));

    metadata.publish_terminal(
        first_load,
        ImageDecodeTerminal::Ready(ResolvedImageMetadata::layout_is_image_pixels(
            48,
            48,
            0,
            false,
            Default::default(),
        )),
    );
    catalog.reconcile_renderer_state(ImageStateEvent::DecodeCompleted(first_load));
    assert!(matches!(
        catalog.lookup(request.clone()),
        ImageLookup::Ready(_)
    ));

    // The renderer's LRU dropped the texture. Its lifecycle notification
    // removes residency metadata before asking the catalog to reconcile.
    metadata.remove_terminal(first_load);
    catalog.reconcile_renderer_state(ImageStateEvent::Evicted(id));

    let ImageLookup::Pending(reloading) = catalog.lookup(request) else {
        panic!("evicted avatar should remain pending until its reload completes");
    };
    assert!(matches!(
        cmd_rx.try_recv().expect("evicted avatar reload"),
        RenderCommand::Asset(AssetCommand::ImageLoadFile { load, .. })
            if load.image() == id && load != first_load
    ));
    assert_eq!(reloading.placement().image_id(), id);
    assert_eq!(reloading.placement().width(), 48);
    assert_eq!(reloading.placement().height(), 48);
}

#[test]
fn eviction_after_decode_but_before_evaluator_service_does_not_strand_pending_image() {
    let (cmd_tx, cmd_rx) = crossbeam_channel::unbounded();
    let metadata = Arc::new(ImageRenderState::default());
    let catalog = AsyncImageCatalog::new(cmd_tx, None, metadata, None);
    let request = file_request("/tmp/large-chat-photo.png");

    let ImageLookup::Pending(first_load) = catalog.lookup(request.clone()) else {
        panic!("new image should begin pending");
    };
    let id = first_load.placement().image_id();
    let first_token = first_load.load();
    assert!(cmd_rx.try_recv().is_ok(), "initial load was queued");

    // The renderer can publish Ready and then evict the same image in one
    // batch. By the time the evaluator services both ordered events, the
    // shared metadata map is already empty; the typed eviction reason must
    // still move Pending -> Evicted instead of leaving it pending forever.
    catalog.reconcile_renderer_state(ImageStateEvent::DecodeCompleted(first_token));
    catalog.reconcile_renderer_state(ImageStateEvent::Evicted(id));

    let ImageLookup::Pending(reload) = catalog.lookup(request) else {
        panic!("visible evicted image should schedule another load");
    };
    assert_eq!(reload.placement().image_id(), id);
    assert!(matches!(
        cmd_rx.try_recv().expect("replacement load"),
        RenderCommand::Asset(AssetCommand::ImageLoadFile { load, .. })
            if load.image() == id && load != first_token
    ));
}

#[test]
fn stale_decode_completion_cannot_promote_a_replacement_load() {
    use neomacs_display_runtime::render_thread::ImageDecodeTerminal;
    use neovm_core::emacs_core::image_catalog::ResolvedImageMetadata;

    let (cmd_tx, cmd_rx) = crossbeam_channel::unbounded();
    let metadata = Arc::new(ImageRenderState::default());
    let catalog = AsyncImageCatalog::new(cmd_tx, None, Arc::clone(&metadata), None);
    let request = file_request("/tmp/replaced-avatar.png");

    let ImageLookup::Pending(first) = catalog.lookup(request.clone()) else {
        panic!("initial image should be pending");
    };
    let first_load = first.load();
    cmd_rx.try_recv().expect("initial load command");

    catalog.reconcile_renderer_state(ImageStateEvent::Evicted(first_load.image()));
    let ImageLookup::Pending(replacement) = catalog.lookup(request.clone()) else {
        panic!("eviction should schedule a replacement load");
    };
    let replacement_load = replacement.load();
    assert_eq!(replacement_load.image(), first_load.image());
    assert_ne!(replacement_load, first_load);
    cmd_rx.try_recv().expect("replacement load command");

    metadata.publish_terminal(
        first_load,
        ImageDecodeTerminal::Ready(ResolvedImageMetadata::layout_is_image_pixels(
            120,
            80,
            0,
            false,
            Default::default(),
        )),
    );
    catalog.reconcile_renderer_state(ImageStateEvent::DecodeCompleted(first_load));

    let ImageLookup::Pending(still_replacement) = catalog.lookup(request) else {
        panic!("a stale completion must not promote the replacement attempt");
    };
    assert_eq!(still_replacement.load(), replacement_load);
}

/// The invariant this catalog's header probe exists for: geometry resolves
/// while the decode is still pending — no pixels, no terminal — and it is the
/// geometry the decode reports for the same image.
///
/// The renderer-side half of that equality
/// (`header_layout_equals_the_decoded_layout_for_every_probed_format` and the
/// fixture case beside it, in `neomacs-renderer-wgpu`) pins the decode's own
/// answer for this very file, so the slot asserted here is the slot the decode
/// will confirm rather than a slot that moves when the pixels land.
#[test]
fn pending_geometry_resolves_from_the_header_before_any_pixel_exists() {
    let (cmd_tx, cmd_rx) = crossbeam_channel::unbounded();
    let metadata = Arc::new(ImageRenderState::default());
    let (redisplay_tx, redisplay_rx) = crossbeam_channel::unbounded();
    let catalog = AsyncImageCatalog::new(
        cmd_tx,
        None,
        Arc::clone(&metadata),
        Some(RedisplayWaker::new(redisplay_tx, None)),
    );
    let fixture = neomacs_infra::workspace_root().join("test/data/image/blank-100x200.png");
    let mut request = file_request(fixture.to_str().expect("utf8 fixture path"));
    // 100x200 with only a width clamp: the decoded layout is 50x100, where the
    // pre-header placeholder reserves the clamp square 50x50 instead.
    request.size = ImageSizeSpec::new(AxisSize::AtMost(50), AxisSize::Native);

    let ImageLookup::Pending(placeholder) = catalog.lookup(request.clone()) else {
        panic!("a new image lookup begins pending");
    };
    assert_eq!(
        (
            placeholder.placement().width(),
            placeholder.placement().height()
        ),
        (50, 50),
        "the slot starts on the request's pinned placeholder"
    );
    let RenderCommand::Asset(AssetCommand::ImageLoadFile { load, .. }) =
        cmd_rx.try_recv().expect("image load command")
    else {
        panic!("a file source loads through ImageLoadFile");
    };
    assert!(
        metadata.terminal(load).is_none(),
        "no decode terminal exists yet"
    );

    // The probe runs off-thread; wait for it without ever consulting the
    // renderer, which is what makes the resolution independent of the decode.
    let deadline = Instant::now() + Duration::from_secs(10);
    let pending = loop {
        let lookup = catalog.lookup(request.clone());
        if lookup.placement().height() == 100 {
            break lookup;
        }
        assert!(
            Instant::now() < deadline,
            "the header probe never resolved this image's geometry"
        );
        std::thread::sleep(Duration::from_millis(5));
    };

    assert!(
        matches!(pending, ImageLookup::Pending(_)),
        "geometry must resolve while the decode is still pending"
    );
    assert!(
        metadata.terminal(load).is_none(),
        "no pixel exists when the geometry does"
    );
    assert_eq!(
        (pending.placement().width(), pending.placement().height()),
        (50, 100),
        "the slot is the decoded geometry, not the placeholder"
    );
    assert_eq!(
        pending.placement().image_id(),
        load.image(),
        "the slot keeps its identity across the refinement"
    );
    assert!(
        matches!(
            redisplay_rx.recv_timeout(Duration::from_secs(1)),
            Ok(neovm_core::keyboard::InputEvent::LayoutInvalidated)
        ),
        "resolved geometry must ask the evaluator to republish layout"
    );
}

/// A source with no readable header keeps the placeholder it always had.
#[test]
fn pending_geometry_without_a_header_keeps_the_request_placeholder() {
    let (cmd_tx, _cmd_rx) = crossbeam_channel::unbounded();
    let metadata = Arc::new(ImageRenderState::default());
    let catalog = AsyncImageCatalog::new(cmd_tx, None, metadata, None);
    let request = file_request("/nonexistent/neomacs/not-an-image.png");

    let ImageLookup::Pending(pending) = catalog.lookup(request.clone()) else {
        panic!("a new image lookup begins pending");
    };
    assert_eq!(
        (pending.placement().width(), pending.placement().height()),
        (24, 24)
    );

    // Let any probe for the unreadable path land before re-checking.
    std::thread::sleep(Duration::from_millis(200));
    let ImageLookup::Pending(unchanged) = catalog.lookup(request) else {
        panic!("an unreadable image stays pending until it fails");
    };
    assert_eq!(
        (
            unchanged.placement().width(),
            unchanged.placement().height()
        ),
        (24, 24),
        "no header means no refinement"
    );
}

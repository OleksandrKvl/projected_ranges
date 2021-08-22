#include <functional>
#include <iostream>
#include <iterator>
#include <ranges>
#include <vector>
#include <algorithm>
#include <type_traits>
#include <concepts>
#include <cassert>

namespace stdf
{
namespace detail
{
template<bool Const, typename T>
using maybe_const_t = std::conditional_t<Const, const T, T>;

template<typename T>
using with_ref = T&;

template<typename T>
concept can_reference = requires
{
    typename with_ref<T>;
};

template<typename T>
concept class_or_enum =
    std::is_class_v<T> || std::is_union_v<T> || std::is_enum_v<T>;

template<typename Range>
inline constexpr auto get_iter_concept()
{
    if constexpr(std::ranges::random_access_range<Range>)
    {
        return std::random_access_iterator_tag{};
    }
    else if constexpr(std::ranges::bidirectional_range<Range>)
    {
        return std::bidirectional_iterator_tag{};
    }
    else if constexpr(std::ranges::forward_range<Range>)
    {
        return std::forward_iterator_tag{};
    }
    else
    {
        return std::input_iterator_tag{};
    }
}

template<typename BaseRange, typename BaseIter, typename Fp>
inline constexpr auto get_iter_cat()
{
    using Res =
        std::invoke_result_t<Fp&, std::ranges::range_reference_t<BaseRange>>;
    if constexpr(std::is_lvalue_reference_v<Res>)
    {
        using Cat = typename std::iterator_traits<BaseIter>::iterator_category;
        if constexpr(std::derived_from<Cat, std::contiguous_iterator_tag>)
        {
            return std::random_access_iterator_tag{};
        }
        else
        {
            return Cat{};
        }
    }
    else
    {
        return std::input_iterator_tag{};
    }
}
} // namespace detail

// improved iter_move() with "dereferenced" version
namespace iter_move_cpo
{
void iter_move();

template<typename From>
concept has_adl_iter_move = detail::class_or_enum<std::remove_cvref_t<From>> &&
    requires(From&& from)
{
    iter_move(static_cast<From&&>(from));
};

struct impl
{
private:
    template<typename From>
    struct result
    {
        using type = std::iter_reference_t<From>;
    };

    template<typename From>
    requires has_adl_iter_move<From>
    struct result<From>
    {
        using type = decltype(iter_move(std::declval<From>()));
    };

    // clang-format off
    template<typename From>
    requires(
        !has_adl_iter_move<From> &&
        std::is_lvalue_reference_v<
            std::iter_reference_t<From>>)
    struct result<From>
    {
        using type = std::remove_cvref_t<std::iter_reference_t<From>>&&;
    };
    // clang-format on

    template<typename From>
    using result_t = typename result<From>::type;

    template<typename From>
    static constexpr bool is_noexcept()
    {
        if constexpr(has_adl_iter_move<From>)
        {
            return noexcept(iter_move(std::declval<From>()));
        }
        else
        {
            return noexcept(*std::declval<From>());
        }
    }

    template<typename From>
    static constexpr bool is_noexcept2()
    {
        if constexpr(has_adl_iter_move<From>)
        {
            return noexcept(iter_move(std::declval<From>()));
        }
        else if constexpr(!std::is_reference_v<std::iter_reference_t<From>>)
        {
            // if operator*() returns by-value then second version of
            // iter_move() move-constructs returned value from `dereferenced`
            return std::is_nothrow_constructible_v<
                result_t<From>,
                std::iter_reference_t<From>&>;
        }
        else
        {
            return true;
        }
    }

public:
    template<typename From>
    constexpr result_t<From> operator()(From&& from) const
        noexcept(is_noexcept<From>())
    {
        if constexpr(has_adl_iter_move<From>)
        {
            return iter_move(static_cast<From&&>(from));
        }
        else if constexpr(std::is_lvalue_reference_v<
                              std::iter_reference_t<From>>)
        {
            return std::move(*from);
        }
        else
        {
            return *from;
        }
    }

    template<typename From>
    constexpr result_t<From>
        operator()(From&& from, std::iter_reference_t<From>& dereferenced) const
        noexcept(is_noexcept2<From>())
    {
        if constexpr(has_adl_iter_move<From>)
        {
            return iter_move(static_cast<From&&>(from));
        }
        else
        {
            return std::move(dereferenced);
        }
    }
};
} // namespace iter_move_cpo

inline namespace cpo
{
inline constexpr iter_move_cpo::impl iter_move{};
}

namespace iter_assign_from_cpo
{
void iter_assign_from();

template<typename To, typename From>
concept has_adl_iter_assign_from =
    detail::class_or_enum<std::remove_cvref_t<To>> &&
    requires(To&& to, From&& from)
{
    iter_assign_from(static_cast<To&&>(to), static_cast<From&&>(from));
};

struct impl
{
private:
    template<typename To, typename From>
    static constexpr bool is_noexcept()
    {
        if constexpr(has_adl_iter_assign_from<To, From>)
        {
            return noexcept(
                iter_assign_from(std::declval<To>(), std::declval<From>()));
        }
        else
        {
            return noexcept((*std::declval<To>()) = std::declval<From>());
        }
    }

    template<typename To, typename From, typename D>
    static constexpr bool is_noexcept2()
    {
        if constexpr(has_adl_iter_assign_from<To, From>)
        {
            return noexcept(
                iter_assign_from(std::declval<To>(), std::declval<From>()));
        }
        else
        {
            return noexcept((std::declval<D>()) = std::declval<From>());
        }
    }

public:
    template<typename To, typename From>
    requires(
        has_adl_iter_assign_from<To, From> ||
        std::indirectly_writable<To, From>) constexpr void
        operator()(To&& to, From&& from) const noexcept(is_noexcept<To, From>())
    {
        if constexpr(has_adl_iter_assign_from<To, From>)
        {
            iter_assign_from(static_cast<To&&>(to), static_cast<From&&>(from));
        }
        else
        {
            *to = static_cast<From&&>(from);
        }
    }

    template<typename To, typename From, typename D>
    requires(
        has_adl_iter_assign_from<To, From> ||
        std::indirectly_writable<To, From>) constexpr void
        operator()(To&& to, From&& from, D&& dereferenced) const
        noexcept(is_noexcept2<To, From, D>())
    {
        if constexpr(has_adl_iter_assign_from<To, From>)
        {
            iter_assign_from(static_cast<To&&>(to), static_cast<From&&>(from));
        }
        else
        {
            dereferenced = static_cast<From&&>(from);
        }
    }
};
} // namespace iter_assign_from_cpo

inline namespace cpo
{
inline constexpr iter_assign_from_cpo::impl iter_assign_from{};
}

template<typename To, typename From>
concept iter_assignable_from = requires(To&& to, From&& from)
{
    stdf::iter_assign_from(static_cast<To&&>(to), static_cast<From&&>(from));
};

namespace iter_copy_root_cpo
{
    void iter_copy_root();

    template<typename From>
    concept has_adl_iter_copy_root =
        detail::class_or_enum<std::remove_cvref_t<From>> &&
        requires(From && from)
    {
        iter_copy_root(static_cast<From&&>(from));
    };

    struct impl
    {
    private:
        template<typename From>
        static constexpr bool is_noexcept()
        {
            if constexpr(has_adl_iter_copy_root<From>)
            {
                return noexcept(iter_copy_root(std::declval<From>()));
            }
            else
            {
                return noexcept(*std::declval<From>());
            }
        }
        template<typename From>
        static constexpr bool is_noexcept2()
        {
            if constexpr(has_adl_iter_copy_root<From>)
            {
                return noexcept(iter_copy_root(std::declval<From>()));
            }
            else
            {
                return true;
            }
        }

        template<typename From>
        struct result
        {
            using type = decltype(*std::declval<From>());
        };

        template<typename From>
        requires has_adl_iter_copy_root<From>
        struct result<From>
        {
            using type = decltype(iter_copy_root(std::declval<From>()));
        };

        template<typename From>
        using result_t = typename result<From>::type;

    public:
        template<typename From>
        constexpr result_t<From> operator()(From&& from) const
            noexcept(is_noexcept<From>())
        {
            if constexpr(has_adl_iter_copy_root<From>)
            {
                return iter_copy_root(static_cast<From&&>(from));
            }
            else
            {
                return *from;
            }
        }

        template<typename From, typename D>
        constexpr result_t<From> operator()(From&& from, D&& dereferenced) const
            noexcept(is_noexcept2<From>())
        {
            if constexpr(has_adl_iter_copy_root<From>)
            {
                return iter_copy_root(static_cast<From&&>(from));
            }
            else
            {
                return std::forward<D>(dereferenced);
            }
        }
    };
} // namespace iter_copy_root_cpo

inline namespace cpo
{
inline constexpr iter_copy_root_cpo::impl iter_copy_root{};
}

template<typename T>
using iter_root_t =
    std::remove_cvref_t<decltype(iter_copy_root(std::declval<T>()))>;

template<typename T>
using iter_root_reference_t = decltype(iter_copy_root(std::declval<T>()));

template<typename T>
using range_root_t = iter_root_t<std::ranges::iterator_t<T>>;

template<typename T>
using range_root_reference_t =
    iter_root_reference_t<std::ranges::iterator_t<T>>;

namespace iter_move_root_cpo
{
void iter_move_root();

template<typename From>
concept has_adl_iter_move_root =
    detail::class_or_enum<std::remove_cvref_t<From>> && requires(From&& from)
{
    iter_move_root(static_cast<From&&>(from));
};

struct impl
{
private:
    template<typename From>
    static constexpr bool is_noexcept()
    {
        if constexpr(has_adl_iter_move_root<From>)
        {
            return noexcept(iter_move_root(std::declval<From>()));
        }
        else
        {
            return noexcept(iter_copy_root(std::declval<From>()));
        }
    }

    template<typename From, typename D>
    static constexpr bool is_noexcept2()
    {
        if constexpr(has_adl_iter_move_root<From>)
        {
            return noexcept(iter_move_root(std::declval<From>()));
        }
        else
        {
            return noexcept(
                iter_copy_root(std::declval<From>(), std::declval<D>()));
        }
    }

    template<typename From>
    struct result
    {
        using type = iter_root_reference_t<From>;
    };

    template<typename From>
    requires has_adl_iter_move_root<From>
    struct result<From>
    {
        using type = decltype(iter_move_root(std::declval<From>()));
    };

    template<typename From>
    requires(
        !has_adl_iter_move_root<From> &&
        std::is_lvalue_reference_v<
            iter_root_reference_t<From>>) struct result<From>
    {
        using type = std::remove_reference_t<iter_root_reference_t<From>>&&;
    };

    template<typename From>
    using result_t = typename result<From>::type;

public:
    template<typename From>
    constexpr result_t<From> operator()(From&& from) const
        noexcept(is_noexcept<From>())
    {
        if constexpr(has_adl_iter_move_root<From>)
        {
            return iter_move_root(static_cast<From&&>(from));
        }
        else if constexpr(std::is_lvalue_reference_v<
                              iter_root_reference_t<From>>)
        {
            return std::move(iter_copy_root(static_cast<From&&>(from)));
        }
        else
        {
            return iter_copy_root(static_cast<From&&>(from));
        }
    }

    template<typename From, typename D>
    constexpr result_t<From> operator()(From&& from, D&& dereferenced) const
        noexcept(is_noexcept2<From, D>())
    {
        if constexpr(has_adl_iter_move_root<From>)
        {
            return iter_move_root(static_cast<From&&>(from));
        }
        else if constexpr(std::is_lvalue_reference_v<
                              iter_root_reference_t<From>>)
        {
            return std::move(iter_copy_root(
                static_cast<From&&>(from), static_cast<D&&>(dereferenced)));
        }
        else
        {
            return iter_copy_root(
                static_cast<From&&>(from), static_cast<D&&>(dereferenced));
        }
    }
};
} // namespace iter_move_root_cpo

inline namespace cpo
{
inline constexpr iter_move_root_cpo::impl iter_move_root{};
}

template<typename T>
using iter_root_rvalue_reference_t =
    decltype(iter_move_root(std::declval<T>()));

template<typename T>
using range_root_rvalue_reference_t =
    iter_root_rvalue_reference_t<std::ranges::iterator_t<T>>;

// analog of std::indirectly_writable
template<typename Out, typename T>
concept iter_writable = requires(Out&& o, T&& t)
{
    iter_assign_from(o, std::forward<T>(t));
    iter_assign_from(std::forward<Out>(o), std::forward<T>(t));
    // no need to check const_cast-ed types like `indirectly_writable` does
    // because it's already handled by `iter_assign_from`.
};

// analog of std::indirectly_readable
// clang-format off
template<typename T>
concept iter_root_readable = requires(const T it)
{
    typename iter_root_t<T>;
    typename iter_root_reference_t<T>;
    typename iter_root_rvalue_reference_t<T>;
    { iter_copy_root(it) } -> std::same_as<iter_root_reference_t<T>>;
    { iter_move_root(it) } -> std::same_as<iter_root_rvalue_reference_t<T>>;
} &&
std::common_reference_with<
    iter_root_reference_t<T>&&, iter_root_t<T>&
> &&
std::common_reference_with<
    iter_root_reference_t<T>&&, iter_root_rvalue_reference_t<T>&&
> &&
std::common_reference_with<
    iter_root_rvalue_reference_t<T>&&, const iter_root_t<T>&
>;
// clang-format on

// analog of std::indirectly_movable
template<typename In, typename Out>
concept iter_root_movable = iter_root_readable<In> &&
    iter_writable<Out, iter_root_rvalue_reference_t<In>>;

// analog of std::indirectly_movable_storable
// clang-format off
template<typename In, typename Out>
concept iter_movable_storable =
    iter_root_movable<In, Out> &&
    iter_writable<Out, iter_root_t<In>> &&
    std::movable<iter_root_t<In>> &&
    std::constructible_from<
        iter_root_t<In>, iter_root_rvalue_reference_t<In>> &&
    std::assignable_from<
        iter_root_t<In>&, iter_root_rvalue_reference_t<In> >;
// clang-format on

// improved `std::ranges::iter_swap()` with dereferenced version
namespace iter_swap_cpo
{
template<typename It1, typename It2>
void iter_swap(It1, It2) = delete;

template<typename It1, typename It2>
concept has_adl_iter_swap =
    (detail::class_or_enum<std::remove_cvref_t<It1>> ||
     detail::class_or_enum<
         std::remove_cvref_t<It2>>)&&requires(It1&& it1, It2&& it2)
{
    iter_swap(static_cast<It1&&>(it1), static_cast<It2&&>(it2));
};

struct impl
{
private:
    template<class It1, class It2>
    static constexpr iter_root_t<It1>
        iter_exchange_move(It1&& it1, It2&& it2) noexcept(
            noexcept(iter_root_t<It1>(iter_move_root(it1))) && noexcept(
                iter_assign_from(it1, iter_move_root(it2))))
    {
        iter_root_t<It1> old_value(iter_move_root(it1));
        iter_assign_from(it1, iter_move_root(it2));
        return old_value;
    }

    template<typename It1, typename It2, typename D1, typename D2>
    static constexpr iter_root_t<It1>
        iter_exchange_move(It1&& it1, It2&& it2, D1&& d1, D2&& d2) noexcept(
            noexcept(iter_root_t<It1>(iter_move_root(it1, d1))) && noexcept(
                iter_assign_from(it1, iter_move_root(it2, d2), d1)))
    {
        iter_root_t<It1> old_value(iter_move_root(it1, d1));
        iter_assign_from(it1, iter_move_root(it2, d2), d1);
        return old_value;
    }

    template<typename It1, typename It2>
    static constexpr bool is_noexcept()
    {
        if constexpr(has_adl_iter_swap<It1, It2>)
        {
            return noexcept(
                iter_swap(std::declval<It1>(), std::declval<It2>()));
        }
        else if constexpr(
            std::indirectly_readable<It1> && std::indirectly_readable<It2> &&
            std::swappable_with<
                std::iter_reference_t<It1>,
                std::iter_reference_t<It2>>)
        {
            return noexcept(
                std::ranges::swap(*std::declval<It1>(), *std::declval<It2>()));
        }
        else
        {
            return noexcept(iter_assign_from(
                std::declval<It1>(),
                iter_exchange_move(std::declval<It2>(), std::declval<It1>())));
        }
    }

    template<typename It1, typename It2, typename D1, typename D2>
    static constexpr bool is_noexcept2()
    {
        if constexpr(has_adl_iter_swap<It1, It2>)
        {
            return noexcept(
                iter_swap(std::declval<It1>(), std::declval<It2>()));
        }
        else if constexpr(
            std::indirectly_readable<It1> && std::indirectly_readable<It2> &&
            std::swappable_with<
                std::iter_reference_t<It1>,
                std::iter_reference_t<It2>>)
        {
            return noexcept(
                std::ranges::swap(std::declval<D1>(), std::declval<D2>()));
        }
        else
        {
            return noexcept(iter_assign_from(
                std::declval<It1>(),
                iter_exchange_move(
                    std::declval<It2>(),
                    std::declval<It1>(),
                    std::declval<D1>(),
                    std::declval<D2>()),
                std::declval<D1>()));
        }
    }

public:
    template<typename It1, typename It2>
    constexpr void operator()(It1&& it1, It2&& it2) const
        noexcept(is_noexcept<It1, It2>()) requires(
            has_adl_iter_swap<It1, It2> ||
            (std::indirectly_readable<It1> && std::indirectly_readable<It2> &&
             std::swappable_with<
                 std::iter_reference_t<It1>,
                 std::iter_reference_t<It2>>) ||
            (iter_movable_storable<It1, It2> &&
             iter_movable_storable<It2, It1>))
    {
        if constexpr(has_adl_iter_swap<It1, It2>)
        {
            iter_swap(static_cast<It1&&>(it1), static_cast<It2&&>(it2));
        }
        else if constexpr(
            std::indirectly_readable<It1> && std::indirectly_readable<It2> &&
            std::swappable_with<
                std::iter_reference_t<It1>,
                std::iter_reference_t<It2>>)
        {
            std::ranges::swap(*it1, *it2);
        }
        else
        {
            iter_assign_from(it1, iter_exchange_move(it2, it1));
        }
    }

    template<typename It1, typename It2, typename D1, typename D2>
    constexpr void operator()(It1&& it1, It2&& it2, D1&& d1, D2&& d2) const
        noexcept(is_noexcept2<It1, It2, D1, D2>()) requires(
            has_adl_iter_swap<It1, It2> ||
            (std::indirectly_readable<It1> && std::indirectly_readable<It2> &&
             std::swappable_with<
                 std::iter_reference_t<It1>,
                 std::iter_reference_t<It2>>) ||
            (iter_movable_storable<It1, It2> &&
             iter_movable_storable<It2, It1>))
    {
        if constexpr(has_adl_iter_swap<It1, It2>)
        {
            iter_swap(static_cast<It1&&>(it1), static_cast<It2&&>(it2));
        }
        else if constexpr(
            std::indirectly_readable<It1> && std::indirectly_readable<It2> &&
            std::swappable_with<
                std::iter_reference_t<It1>,
                std::iter_reference_t<It2>>)
        {
            std::ranges::swap(static_cast<D1&&>(d1), static_cast<D2&&>(d2));
        }
        else
        {
            iter_assign_from(it1, iter_exchange_move(it2, it1, d1, d2), d1);
        }
    }
};
} // namespace iter_swap_cpo

inline namespace cpo
{
inline constexpr iter_swap_cpo::impl iter_swap{};
}

// analog of std::indirectly_swappable
template<class I1, class I2>
concept iter_swappable = iter_root_readable<I1> && iter_root_readable<I2> &&
    requires(const I1 i1, const I2 i2)
{
    stdf::iter_swap(i1, i1);
    stdf::iter_swap(i1, i2);
    stdf::iter_swap(i2, i1);
    stdf::iter_swap(i2, i2);
};

//------------------------------------------------------------------------------

template<std::ranges::input_range Range, std::copy_constructible Fp>
requires std::ranges::view<Range> && std::is_object_v<Fp> &&
    std::regular_invocable<Fp&, std::ranges::range_reference_t<Range>> &&
    detail::can_reference<
        std::invoke_result_t<Fp&, std::ranges::range_reference_t<Range>>>
class projection_view
    : public std::ranges::view_interface<projection_view<Range, Fp>>
{
private:
    template<bool IsConst>
    class Sentinel;

    // ITERATOR ----------------------------------------------------------------

    template<bool IsConst>
    class Iterator
    {
    private:
        using ParentView = detail::maybe_const_t<IsConst, projection_view>;
        using BaseRange = detail::maybe_const_t<IsConst, Range>;

        using BaseIter = std::ranges::iterator_t<BaseRange>;

        BaseIter current{};
        ParentView* parent{};

        template<typename It>
        struct root_type_impl
        {
            using type = It;
        };

        template<typename It>
        requires requires(It it)
        {
            it.root();
        }
        struct root_type_impl<It>
        {
            using type = decltype(std::declval<It>().root());
        };

    public:
        using iterator_concept = decltype(detail::get_iter_concept<Range>());
        using iterator_category =
            decltype(detail::get_iter_cat<BaseRange, BaseIter, Fp>());
        using value_type = std::remove_cvref_t<std::invoke_result_t<
            Fp&,
            std::ranges::range_reference_t<BaseRange>>>;
        using difference_type = std::ranges::range_difference_t<BaseRange>;
        using root_type = typename root_type_impl<BaseIter>::type;

        Iterator() = default;

        constexpr Iterator(ParentView& parent, BaseIter current)
            : current{std::move(current)}, parent{std::addressof(parent)}
        {
        }

        constexpr Iterator(Iterator<!IsConst> i) requires IsConst
            && std::convertible_to<std::ranges::iterator_t<Range>, BaseIter>
            : current{std::move(i.current)}, parent{i.parent}
        {
        }

        constexpr BaseIter base() const& requires std::copyable<BaseIter>
        {
            return current;
        }

        constexpr BaseIter base() &&
        {
            return std::move(current);
        }

        constexpr decltype(auto) operator*() const
            noexcept(noexcept(std::invoke(parent->fun, *current)))
        {
            return std::invoke(parent->fun, *current);
        }

        constexpr Iterator& operator++()
        {
            ++current;
            return *this;
        }

        constexpr void operator++(int)
        {
            ++current;
        }

        constexpr Iterator
            operator++(int) requires std::ranges::forward_range<BaseRange>
        {
            auto tmp = *this;
            ++*this;
            return tmp;
        }

        constexpr Iterator&
            operator--() requires std::ranges::bidirectional_range<BaseRange>
        {
            --current;
            return *this;
        }

        constexpr Iterator
            operator--(int) requires std::ranges::bidirectional_range<BaseRange>
        {
            auto tmp = *this;
            --*this;
            return tmp;
        }

        constexpr Iterator& operator+=(difference_type n) requires
            std::ranges::random_access_range<BaseRange>
        {
            current += n;
            return *this;
        }

        constexpr Iterator& operator-=(difference_type n) requires
            std::ranges::random_access_range<BaseRange>
        {
            current -= n;
            return *this;
        }

        constexpr decltype(auto) operator[](difference_type n)
            const requires std::ranges::random_access_range<BaseRange>
        {
            return std::invoke(parent->fun, current[n]);
        }

        friend constexpr bool operator==(
            const Iterator& x,
            const Iterator& y) requires std::equality_comparable<BaseIter>
        {
            return x.current == y.current;
        }

        friend constexpr bool
            operator<(const Iterator& x, const Iterator& y) requires
            std::ranges::random_access_range<BaseRange>
        {
            return x.current < y.current;
        }

        friend constexpr bool
            operator>(const Iterator& x, const Iterator& y) requires
            std::ranges::random_access_range<BaseRange>
        {
            return y < x;
        }

        friend constexpr bool
            operator<=(const Iterator& x, const Iterator& y) requires
            std::ranges::random_access_range<BaseRange>
        {
            return !(y < x);
        }

        friend constexpr bool
            operator>=(const Iterator& x, const Iterator& y) requires
            std::ranges::random_access_range<BaseRange>
        {
            return !(x < y);
        }

        friend constexpr auto
            operator<=>(const Iterator& x, const Iterator& y) requires
            std::ranges::random_access_range<BaseRange> &&
            std::three_way_comparable<BaseIter>
        {
            return x.current <=> y.current;
        }

        friend constexpr Iterator
            operator+(Iterator i, difference_type n) requires
            std::ranges::random_access_range<BaseRange>
        {
            return {*i.parent, i.current + n};
        }

        friend constexpr Iterator operator+(
            difference_type n,
            Iterator i) requires std::ranges::random_access_range<BaseRange>
        {
            return {*i.parent, i.current + n};
        }

        friend constexpr Iterator
            operator-(Iterator i, difference_type n) requires
            std::ranges::random_access_range<BaseRange>
        {
            return {*i.parent, i.current - n};
        }

        friend constexpr difference_type
            operator-(const Iterator& x, const Iterator& y) requires
            std::ranges::random_access_range<BaseRange>
        {
            return x.current - y.current;
        }

        friend constexpr void
            iter_swap(const Iterator& x, const Iterator& y) noexcept(
                noexcept(stdf::iter_swap(x.current, y.current))) requires
            stdf::iter_swappable<BaseIter, BaseIter>
        {
            return stdf::iter_swap(x.current, y.current);
        }

        friend constexpr decltype(iter_copy_root(std::declval<BaseIter>()))
            iter_copy_root(const Iterator& it) noexcept
        {
            return iter_copy_root(it.current);
        }

        friend constexpr decltype(iter_move_root(std::declval<BaseIter>()))
            iter_move_root(const Iterator& it) noexcept
        {
            return iter_move_root(it.current);
        }

        template<typename T>
        requires iter_assignable_from<BaseIter, T&&>
        friend constexpr void
            iter_assign_from(const Iterator& it, T&& val) noexcept(
                noexcept(stdf::iter_assign_from(
                    std::declval<BaseIter>(), std::declval<T&&>())))
        {
            stdf::iter_assign_from(it.current, std::forward<T>(val));
        }

        constexpr root_type root() const noexcept
        {
            if constexpr(requires { current.root(); })
            {
                return current.root();
            }
            else
            {
                return current;
            }
        }

        friend Iterator<!IsConst>;

        template<bool>
        friend class Sentinel;
    };
    // ITERATOR ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    // SENTINEL ----------------------------------------------------------------
    template<bool IsConst>
    class Sentinel
    {
    private:
        using Parent = detail::maybe_const_t<IsConst, projection_view>;
        using Base = detail::maybe_const_t<IsConst, Range>;

        template<bool Const2>
        constexpr std::ranges::range_difference_t<Base>
            distance_from(const Iterator<Const2>& i) const
        {
            return end - i.current;
        }

        template<bool Const2>
        constexpr bool equal(const Iterator<Const2>& i) const
        {
            return i.current == end;
        }

        std::ranges::sentinel_t<Base> end{};

    public:
        Sentinel() = default;

        constexpr explicit Sentinel(std::ranges::sentinel_t<Base> end)
            : end{end}
        {
        }

        constexpr Sentinel(Sentinel<!IsConst> i) requires IsConst
            && std::convertible_to<
                std::ranges::sentinel_t<Range>,
                std::ranges::sentinel_t<Base>> : end(std::move(i.end))
        {
        }

        constexpr std::ranges::sentinel_t<Base> base() const
        {
            return end;
        }

        template<bool Const2>
        requires std::sentinel_for<
            std::ranges::sentinel_t<Base>,
            std::ranges::iterator_t<detail::maybe_const_t<Const2, Range>>>
        friend constexpr bool
            operator==(const Iterator<Const2>& x, const Sentinel& y)
        {
            return y.equal(x);
        }

        template<bool Const2>
        requires std::sized_sentinel_for<
            std::ranges::sentinel_t<Base>,
            std::ranges::iterator_t<detail::maybe_const_t<Const2, Range>>>
        friend constexpr std::ranges::range_difference_t<Base>
            operator-(const Iterator<Const2>& x, const Sentinel& y)
        {
            return -y.distance_from(x);
        }

        template<bool Const2>
        requires std::sized_sentinel_for<
            std::ranges::sentinel_t<Base>,
            std::ranges::iterator_t<detail::maybe_const_t<Const2, Range>>>
        friend constexpr std::ranges::range_difference_t<Base>
            operator-(const Sentinel& y, const Iterator<Const2>& x)
        {
            return y.distance_from(x);
        }

        friend Sentinel<!IsConst>;
    };
    // SENTINEL ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Range baseRange{};
    Fp fun;

public:
    projection_view() = default;

    constexpr projection_view(Range base, Fp fun)
        : baseRange{std::move(base)}, fun{std::move(fun)}
    {
    }

    constexpr Range base() const& requires std::copy_constructible<Range>
    {
        return baseRange;
    }

    constexpr Range base() &&
    {
        return std::move(baseRange);
    }

    constexpr Iterator<false> begin()
    {
        return Iterator<false>{*this, std::ranges::begin(baseRange)};
    }

    constexpr Iterator<true> begin() const requires
        std::ranges::range<const Range> && std::regular_invocable<
            const Fp&,
            std::ranges::range_reference_t<const Range>>
    {
        return Iterator<true>{*this, std::ranges::begin(baseRange)};
    }

    constexpr Sentinel<false> end()
    {
        return Sentinel<false>{std::ranges::end(baseRange)};
    }

    constexpr Iterator<false> end() requires std::ranges::common_range<Range>
    {
        return Iterator<false>{*this, std::ranges::end(baseRange)};
    }

    constexpr Sentinel<true> end() const requires
        std::ranges::range<const Range> && std::regular_invocable<
            const Fp&,
            std::ranges::range_reference_t<const Range>>
    {
        return Sentinel<true>{std::ranges::end(baseRange)};
    }

    constexpr Iterator<true> end() const requires
        std::ranges::common_range<const Range> && std::regular_invocable<
            const Fp&,
            std::ranges::range_reference_t<const Range>>
    {
        return Iterator<true>{*this, std::ranges::end(baseRange)};
    }

    constexpr auto size() requires std::ranges::sized_range<Range>
    {
        return std::ranges::size(baseRange);
    }

    constexpr auto size() const requires std::ranges::sized_range<const Range>
    {
        return std::ranges::size(baseRange);
    }
};

template<typename Range, typename Fp>
projection_view(Range&&, Fp) -> projection_view<std::views::all_t<Range>, Fp>;

namespace views
{
template<typename F>
class projection
{
public:
    explicit constexpr projection(F f) : p{std::move(f)}
    {
    }

    template<std::ranges::input_range R>
    friend constexpr auto operator|(R&& r, projection&& p)
    {
        return projection_view{std::forward<R>(r), std::move(p.p)};
    }

private:
    F p;
};

// inline constexpr std::ranges::views::__adaptor::_RangeAdaptor projection
// =
//     []<std::ranges::viewable_range Range, typename Fp>(Range&& r, Fp&& f)
// {
//     return projection_view{std::forward<Range>(r), std::forward<Fp>(f)};
// };

} // namespace views

template<std::ranges::input_range Range, std::copy_constructible Fp>
requires std::ranges::view<Range> && std::is_object_v<Fp> &&
    std::regular_invocable<Fp&, std::ranges::range_reference_t<Range>> &&
    detail::can_reference<
        std::invoke_result_t<Fp&, std::ranges::range_reference_t<Range>>>
class narrow_projection_view
    : public std::ranges::view_interface<narrow_projection_view<Range, Fp>>
{
private:
    template<bool IsConst>
    class Sentinel;

    template<bool IsConst>
    class Iterator
    {
    private:
        using Parent = detail::maybe_const_t<IsConst, narrow_projection_view>;
        using Base = detail::maybe_const_t<IsConst, Range>;
        using BaseIter = std::ranges::iterator_t<Base>;

        BaseIter current = BaseIter();
        Parent* parent{};

    public:
        using iterator_concept = decltype(detail::get_iter_concept<Range>());
        using iterator_category =
            decltype(detail::get_iter_cat<Base, BaseIter, Fp>());
        using value_type = std::remove_cvref_t<
            std::invoke_result_t<Fp&, std::ranges::range_reference_t<Base>>>;
        using difference_type = std::ranges::range_difference_t<Base>;

        Iterator() = default;

        constexpr Iterator(Parent& parent, BaseIter current)
            : current(std::move(current)), parent(std::addressof(parent))
        {
        }

        constexpr Iterator(Iterator<!IsConst> i) requires IsConst
            && std::convertible_to<std::ranges::iterator_t<Range>, BaseIter>
            : current(std::move(i.current)), parent(i.parent)
        {
        }

        constexpr BaseIter base() const& requires std::copyable<BaseIter>
        {
            return current;
        }

        constexpr BaseIter base() &&
        {
            return std::move(current);
        }

        constexpr decltype(auto) operator*() const
            noexcept(noexcept(std::invoke(parent->fun, *current)))
        {
            return std::invoke(parent->fun, *current);
        }

        constexpr Iterator& operator++()
        {
            ++current;
            return *this;
        }

        constexpr void operator++(int)
        {
            ++current;
        }

        constexpr Iterator
            operator++(int) requires std::ranges::forward_range<Base>
        {
            auto tmp = *this;
            ++*this;
            return tmp;
        }

        constexpr Iterator&
            operator--() requires std::ranges::bidirectional_range<Base>
        {
            --current;
            return *this;
        }

        constexpr Iterator
            operator--(int) requires std::ranges::bidirectional_range<Base>
        {
            auto tmp = *this;
            --*this;
            return tmp;
        }

        constexpr Iterator& operator+=(
            difference_type n) requires std::ranges::random_access_range<Base>
        {
            current += n;
            return *this;
        }

        constexpr Iterator& operator-=(
            difference_type n) requires std::ranges::random_access_range<Base>
        {
            current -= n;
            return *this;
        }

        constexpr decltype(auto) operator[](difference_type n)
            const requires std::ranges::random_access_range<Base>
        {
            return std::invoke(parent->fun, current[n]);
        }

        friend constexpr bool operator==(
            const Iterator& x,
            const Iterator& y) requires std::equality_comparable<BaseIter>
        {
            return x.current == y.current;
        }

        friend constexpr bool operator<(
            const Iterator& x,
            const Iterator& y) requires std::ranges::random_access_range<Base>
        {
            return x.current < y.current;
        }

        friend constexpr bool operator>(
            const Iterator& x,
            const Iterator& y) requires std::ranges::random_access_range<Base>
        {
            return y < x;
        }

        friend constexpr bool operator<=(
            const Iterator& x,
            const Iterator& y) requires std::ranges::random_access_range<Base>
        {
            return !(y < x);
        }

        friend constexpr bool operator>=(
            const Iterator& x,
            const Iterator& y) requires std::ranges::random_access_range<Base>
        {
            return !(x < y);
        }

        friend constexpr auto
            operator<=>(const Iterator& x, const Iterator& y) requires
            std::ranges::random_access_range<Base> &&
            std::three_way_comparable<BaseIter>
        {
            return x.current <=> y.current;
        }

        friend constexpr Iterator operator+(
            Iterator i,
            difference_type n) requires std::ranges::random_access_range<Base>
        {
            return {*i.parent, i.current + n};
        }

        friend constexpr Iterator operator+(
            difference_type n,
            Iterator i) requires std::ranges::random_access_range<Base>
        {
            return {*i.parent, i.current + n};
        }

        friend constexpr Iterator operator-(
            Iterator i,
            difference_type n) requires std::ranges::random_access_range<Base>
        {
            return {*i.parent, i.current - n};
        }

        friend constexpr difference_type operator-(
            const Iterator& x,
            const Iterator& y) requires std::ranges::random_access_range<Base>
        {
            return x.current - y.current;
        }

        friend constexpr iter_root_reference_t<BaseIter>
            iter_copy_root(const Iterator& it) requires(
                !std::is_lvalue_reference_v<std::iter_reference_t<Iterator>>)
        {
            return stdf::iter_copy_root(it.current);
        }

        template<typename T>
        friend constexpr void
            iter_assign_from(const Iterator& it, T&& v) requires(
                !std::is_lvalue_reference_v<std::iter_reference_t<Iterator>> &&
                stdf::iter_assignable_from<BaseIter, T>)
        {
            stdf::iter_assign_from(it.current, std::forward<T>(v));
        }

        constexpr auto root() const noexcept
        {
            if constexpr(requires { current.root(); })
            {
                return current.root();
            }
            else
            {
                return current;
            }
        }

        friend Iterator<!IsConst>;

        template<bool>
        friend class Sentinel;
    };

    template<bool IsConst>
    class Sentinel
    {
    private:
        using Parent = detail::maybe_const_t<IsConst, narrow_projection_view>;
        using Base = detail::maybe_const_t<IsConst, Range>;

        template<bool Const2>
        constexpr std::ranges::range_difference_t<Base>
            distance_from(const Iterator<Const2>& i) const
        {
            return end - i.current;
        }

        template<bool Const2>
        constexpr bool equal(const Iterator<Const2>& i) const
        {
            return i.current == end;
        }

        std::ranges::sentinel_t<Base> end{};

    public:
        Sentinel() = default;

        constexpr explicit Sentinel(std::ranges::sentinel_t<Base> end)
            : end{end}
        {
        }

        constexpr Sentinel(Sentinel<!IsConst> i) requires IsConst
            && std::convertible_to<
                std::ranges::sentinel_t<Range>,
                std::ranges::sentinel_t<Base>> : end(std::move(i.end))
        {
        }

        constexpr std::ranges::sentinel_t<Base> base() const
        {
            return end;
        }

        template<bool Const2>
        requires std::sentinel_for<
            std::ranges::sentinel_t<Base>,
            std::ranges::iterator_t<detail::maybe_const_t<Const2, Range>>>
        friend constexpr bool
            operator==(const Iterator<Const2>& x, const Sentinel& y)
        {
            return y.equal(x);
        }

        template<bool Const2>
        requires std::sized_sentinel_for<
            std::ranges::sentinel_t<Base>,
            std::ranges::iterator_t<detail::maybe_const_t<Const2, Range>>>
        friend constexpr std::ranges::range_difference_t<Base>
            operator-(const Iterator<Const2>& x, const Sentinel& y)
        {
            return -y.distance_from(x);
        }

        template<bool Const2>
        requires std::sized_sentinel_for<
            std::ranges::sentinel_t<Base>,
            std::ranges::iterator_t<detail::maybe_const_t<Const2, Range>>>
        friend constexpr std::ranges::range_difference_t<Base>
            operator-(const Sentinel& y, const Iterator<Const2>& x)
        {
            return y.distance_from(x);
        }

        friend Sentinel<!IsConst>;
    };

    Range m_base{};
    Fp fun;

public:
    narrow_projection_view() = default;

    constexpr narrow_projection_view(Range base, Fp fun)
        : m_base(std::move(base)), fun(std::move(fun))
    {
    }

    constexpr Range base() const& requires std::copy_constructible<Range>
    {
        return m_base;
    }

    constexpr Range base() &&
    {
        return std::move(m_base);
    }

    constexpr Iterator<false> begin()
    {
        return Iterator<false>{*this, std::ranges::begin(m_base)};
    }

    constexpr Iterator<true> begin() const requires
        std::ranges::range<const Range> && std::regular_invocable<
            const Fp&,
            std::ranges::range_reference_t<const Range>>
    {
        return Iterator<true>{*this, std::ranges::begin(m_base)};
    }

    constexpr Sentinel<false> end()
    {
        return Sentinel<false>{std::ranges::end(m_base)};
    }

    constexpr Iterator<false> end() requires std::ranges::common_range<Range>
    {
        return Iterator<false>{*this, std::ranges::end(m_base)};
    }

    constexpr Sentinel<true> end() const requires
        std::ranges::range<const Range> && std::regular_invocable<
            const Fp&,
            std::ranges::range_reference_t<const Range>>
    {
        return Sentinel<true>{std::ranges::end(m_base)};
    }

    constexpr Iterator<true> end() const requires
        std::ranges::common_range<const Range> && std::regular_invocable<
            const Fp&,
            std::ranges::range_reference_t<const Range>>
    {
        return Iterator<true>{*this, std::ranges::end(m_base)};
    }

    constexpr auto size() requires std::ranges::sized_range<Range>
    {
        return std::ranges::size(m_base);
    }

    constexpr auto size() const requires std::ranges::sized_range<const Range>
    {
        return std::ranges::size(m_base);
    }
};

template<typename Range, typename Fp>
narrow_projection_view(Range&&, Fp)
    -> narrow_projection_view<std::views::all_t<Range>, Fp>;

namespace views
{
template<typename F>
class narrow_projection
{
public:
    explicit constexpr narrow_projection(F f) : p{std::move(f)}
    {
    }

    template<std::ranges::input_range R>
    friend constexpr auto operator|(R&& r, narrow_projection&& p)
    {
        return narrow_projection_view{std::forward<R>(r), std::move(p.p)};
    }

private:
    F p;
};
} // namespace views

//------------------------------------------------------------------------------

// analog of std::indirectly_copyable
template<typename In, typename Out>
concept iter_root_copyable =
    iter_root_readable<In> && iter_writable<Out, iter_root_t<In>>;

template<
    std::ranges::input_range R,
    std::weakly_incrementable O,
    std::indirect_unary_predicate<std::ranges::iterator_t<R>> Pred>
requires std::indirectly_readable<std::ranges::iterator_t<R>> &&
    iter_root_copyable<std::ranges::iterator_t<R>, O>
constexpr std::ranges::copy_if_result<std::ranges::borrowed_iterator_t<R>, O>
    copy_if(R&& in, O out, Pred pred)
{
    auto first = std::ranges::begin(in);
    auto last = std::ranges::end(in);
    for(; first != last; ++first)
    {
        auto&& x = *first;
        if(std::invoke(pred, x))
        {
            iter_assign_from(
                out, iter_copy_root(first, std::forward<decltype(x)>(x)));

            ++out;
        }
    }
    return {std::move(first), std::move(out)};
}

// analog of std::permutable
template<typename I>
concept iter_permutable = std::forward_iterator<I> &&
    iter_movable_storable<I, I> && iter_swappable<I, I>;

// analog of std::sortable
template<typename I, typename R = std::ranges::less>
concept iter_sortable =
    iter_permutable<I> && std::indirect_strict_weak_order<R, I>;

// simple selection sort
template<std::ranges::random_access_range R, typename Cmp = std::ranges::less>
requires std::indirectly_readable<std::ranges::iterator_t<R>> &&
    iter_sortable<std::ranges::iterator_t<R>, Cmp>
void sort(R&& r, Cmp cmp = {})
{
    auto begin = std::ranges::begin(r);
    auto end = std::ranges::end(r);

    for(; begin != end; ++begin)
    {
        auto min = begin;

        for(auto next = std::ranges::next(begin); next != end; ++next)
        {
            if(std::invoke(cmp, *next, *min))
            {
                min = next;
            }
        }
        std::ranges::iter_swap(begin, min);
    }
}

template<
    std::ranges::forward_range R,
    std::indirect_unary_predicate<std::ranges::iterator_t<R>> Pred>
requires std::indirectly_readable<std::ranges::iterator_t<R>> &&
    iter_permutable<std::ranges::iterator_t<R>>
constexpr std::ranges::borrowed_subrange_t<R> remove_if(R&& r, Pred pred)
{
    auto first = std::ranges::begin(r);
    auto last = std::ranges::end(r);

    for(; first != last; ++first)
    {
        if(std::invoke(pred, *first))
        {
            break;
        }
    }

    if(first != last)
    {
        auto i = std::ranges::next(first);
        for(; i != last; ++i)
        {
            auto&& x = *i;
            if(!(std::invoke(pred, x)))
            {
                iter_assign_from(
                    first, iter_move_root(i, std::forward<decltype(x)>(x)));

                ++first;
            }
        }
        return {first, i};
    }

    return {first, first};
}

template<class I, class T>
concept output_iterator =
    std::input_or_output_iterator<I> && iter_writable<I, T>;

template<class R, class T>
concept output_range =
    std::ranges::range<R> && output_iterator<std::ranges::iterator_t<R>, T>;

// originally, fill doesn't support projections at all
template<class T, std::ranges::output_range<const T&> R>
constexpr std::ranges::borrowed_iterator_t<R> fill(R&& r, const T& value)
{
    auto first = std::ranges::begin(r);
    auto last = std::ranges::end(r);
    for(; first != last; ++first)
    {
        iter_assign_from(first, value);
    }
    return first;
}

// clang-format off
template<
    std::ranges::input_range R,
    typename T,
    std::indirect_unary_predicate<std::ranges::iterator_t<R>> Pred>
requires std::indirectly_readable<std::ranges::iterator_t<R>>
    && iter_writable<std::ranges::iterator_t<R>, const T&>
constexpr std::ranges::borrowed_iterator_t<R>
    replace_if(R&& r, Pred pred, const T& new_value)
{
    auto first = std::ranges::begin(r);
    auto last = std::ranges::end(r);
    for(; first != last; ++first)
    {
        auto&& val = *first;
        if(pred(val))
        {
            stdf::iter_assign_from(first, new_value, val);
        }
    }

    return first;
}
// clang-format on
} // namespace stdf

struct Y
{
    int a;
    int b;

    auto operator<=>(const Y&) const = default;
};

struct X
{
    int x;
    Y y;

    auto operator<=>(const X&) const = default;
};

void projection_test()
{
    std::vector<Y> v{{1, 10}, {2, 20}, {3, 30}};
    auto pv = v | stdf::views::projection(&Y::a);

    auto it1 = std::ranges::begin(pv);
    auto it2 = it1 + 1;

    assert(*it1 == 1);
    assert(*it2 == 2);
    static_assert(std::is_same_v<decltype(*it1), int&>);

    assert(std::ranges::iter_move(it1) == 1);
    assert(std::ranges::iter_move(it2) == 2);
    static_assert(std::is_same_v<decltype(std::ranges::iter_move(it1)), int&&>);

    std::ranges::iter_swap(it1, it2);
    assert(*it1 == 2);
    assert(*it2 == 1);
    assert((v[0] == Y{2, 20}));
    assert((v[1] == Y{1, 10}));

    auto copied = stdf::iter_copy_root(it1);
    auto moved = stdf::iter_move_root(it2);
    static_assert(std::is_same_v<decltype(stdf::iter_copy_root(it1)), Y&>);
    static_assert(std::is_same_v<decltype(stdf::iter_move_root(it2)), Y&&>);
    assert((copied == Y{2, 20}));
    assert((moved == Y{1, 10}));

    stdf::iter_assign_from(it1, moved);
    stdf::iter_assign_from(it2, copied);
    assert((v[0] == Y{1, 10}));
    assert((v[1] == Y{2, 20}));

    stdf::iter_assign_from(it1, copied);
    assert((v[0] == Y{2, 20}));

    stdf::iter_assign_from(it1, 1);
    assert((v[0] == Y{1, 20}));
}

void narrow_projection_test()
{
    std::vector<Y> v{{1, 10}, {2, 20}, {3, 30}};
    auto pv = v | stdf::views::narrow_projection(&Y::a);

    auto it1 = std::ranges::begin(pv);
    auto it2 = it1 + 1;

    assert(*it1 == 1);
    assert(*it2 == 2);
    static_assert(std::is_same_v<decltype(*it1), int&>);

    assert(std::ranges::iter_move(it1) == 1);
    assert(std::ranges::iter_move(it2) == 2);
    static_assert(std::is_same_v<decltype(std::ranges::iter_move(it1)), int&&>);

    std::ranges::iter_swap(it1, it2);
    assert(*it1 == 2);
    assert(*it2 == 1);
    assert((v[0] == Y{2, 10}));
    assert((v[1] == Y{1, 20}));

    auto copied = stdf::iter_copy_root(it1);
    auto moved = stdf::iter_move_root(it2);
    static_assert(std::is_same_v<decltype(stdf::iter_copy_root(it1)), int&>);
    static_assert(std::is_same_v<decltype(stdf::iter_move_root(it2)), int&&>);
    assert(copied == 2);
    assert(moved == 1);

    stdf::iter_assign_from(it1, moved);
    stdf::iter_assign_from(it2, copied);
    assert((v[0] == Y{1, 10}));
    assert((v[1] == Y{2, 20}));

    stdf::iter_assign_from(it1, copied);
    assert((v[0] == Y{2, 10}));
}

template<auto N>
struct less_than
{
    constexpr bool operator()(auto&& x) const noexcept
    {
        return x < N;
    }
};

void copy_if_test()
{
    using namespace stdf::views;

    std::vector<X> v1{{1, {10, 100}}, {2, {20, 200}}, {3, {30, 300}}};
    std::vector<X> v2;
    std::vector<Y> v3;
    std::vector<int> v4;
    std::vector<int> v5;
    std::vector<X> v6;

    stdf::copy_if(
        v1 | projection(&X::y),
        std::back_inserter(v2),
        less_than<Y{30, 300}>{});
    assert((v2 == std::vector<X>{{1, {10, 100}}, {2, {20, 200}}}));

    stdf::copy_if(
        v1 | narrow_projection(&X::y),
        std::back_inserter(v3),
        less_than<Y{30, 300}>{});
    assert((v3 == std::vector<Y>{{10, 100}, {20, 200}}));

    stdf::copy_if(
        v1 | narrow_projection(&X::x),
        std::back_inserter(v4),
        less_than<int{3}>{});
    assert((v4 == std::vector<int>{1, 2}));

    stdf::copy_if(
        v1 | narrow_projection(&X::y) | narrow_projection(&Y::b),
        std::back_inserter(v5),
        less_than<int{300}>{});
    assert((v5 == std::vector<int>{100, 200}));

    stdf::copy_if(v1, std::back_inserter(v6), less_than<X{2, {20, 200}}>{});
    assert((v6 == std::vector<X>{{1, {10, 100}}}));
}

void sort_test()
{
    using namespace stdf::views;

    std::vector<X> v{{1, {10, 100}}, {2, {20, 200}}, {3, {30, 300}}};

    stdf::sort(v, std::ranges::greater{});
    assert(
        (v == std::vector<X>{{3, {30, 300}}, {2, {20, 200}}, {1, {10, 100}}}));

    // sort the whole X objects using &X::x member
    stdf::sort(v | projection(&X::x));
    assert(
        (v == std::vector<X>{{1, {10, 100}}, {2, {20, 200}}, {3, {30, 300}}}));

    // sort only X::x
    stdf::sort(v | narrow_projection(&X::x), std::ranges::greater{});
    assert(
        (v == std::vector<X>{{3, {10, 100}}, {2, {20, 200}}, {1, {30, 300}}}));

    // sort only X::Y using &Y::b
    stdf::sort(
        v | narrow_projection(&X::y) | projection(&Y::b),
        std::ranges::greater{});
    assert(
        (v == std::vector<X>{{3, {30, 300}}, {2, {20, 200}}, {1, {10, 100}}}));
}

void remove_if_test()
{
    using namespace stdf::views;

    std::vector<X> v{{1, {10, 100}}, {2, {20, 200}}, {3, {30, 300}}};
    auto v2 = v;

    // we need to store it in a variable, otherwise range returned from
    // `remove_if` would be dangling
    auto pv = v | projection(&X::x);
    auto removed = stdf::remove_if(pv, less_than<int{3}>{});

    v.erase(removed.begin().root(), removed.end().root());
    assert((v == std::vector<X>{{3, {30, 300}}}));

    auto removed2 = stdf::remove_if(v2, less_than<X{3, {30, 300}}>{});
    v2.erase(removed2.begin(), removed2.end());
    assert((v2 == std::vector<X>{{3, {30, 300}}}));
}

void fill_test()
{
    using namespace stdf::views;

    std::vector<X> v{{1, {10, 100}}, {2, {20, 200}}, {3, {30, 300}}};
    auto pv = v | projection(&X::x);

    stdf::fill(pv, 5);
    assert(
        (v == std::vector<X>{{5, {10, 100}}, {5, {20, 200}}, {5, {30, 300}}}));

    stdf::fill(v, X{1, {10, 100}});
    assert(
        (v == std::vector<X>{{1, {10, 100}}, {1, {10, 100}}, {1, {10, 100}}}));
}

void replace_if_test()
{
    using namespace stdf::views;

    std::vector<X> v{{1, {10, 100}}, {2, {20, 200}}, {3, {30, 300}}};

    stdf::replace_if(v | projection(&X::x), less_than<int{3}>{}, 0);
    assert(
        (v == std::vector<X>{{0, {10, 100}}, {0, {20, 200}}, {3, {30, 300}}}));

    // this is possible because `projection` allows to assign both
    // value_type(int) and root_type(X). `narrow_projection` doesn't allow it
    stdf::replace_if(v | projection(&X::x), less_than<int{3}>{}, X{0, {0, 0}});
    assert((v == std::vector<X>{{0, {0, 0}}, {0, {0, 0}}, {3, {30, 300}}}));
}

// struct S
// {
//     int i;
// };

// template<
//     bool DerefByValue,
//     bool CustomIterMove = false,
//     bool CustomIterCopyRoot = false,
//     bool CustomIterMoveRoot = false,
//     bool CustomIterAssign = false,
//     bool CustomIterSwap = false>
// class iterator
// {
// public:
//     using value_type = int;
//     using root_type = S;

//     value_type operator*() requires DerefByValue
//     {
//         deref_calls++;
//         return s.i;
//     }

//     value_type& operator*() requires(!DerefByValue)
//     {
//         deref_calls++;
//         return s.i;
//     }

//     friend value_type&& iter_move(iterator& it) requires CustomIterMove
//     {
//         iter_move_calls++;
//         return std::move(it.s.i);
//     }

//     friend root_type& iter_copy_root(iterator& it) requires
//     CustomIterCopyRoot
//     {
//         iter_copy_root_calls++;
//         return it.s;
//     }

//     friend root_type&& iter_move_root(iterator& it) requires
//     CustomIterMoveRoot
//     {
//         iter_move_root_calls++;
//         return std::move(it.s);
//     }

//     friend void iter_assign_from(const iterator& it, value_type& x) requires
//         CustomIterAssign
//     {
//         iter_assign_from_calls++;
//         return it.s = x;
//     }

//     friend void iter_swap(iterator it1, iterator it2) requires CustomIterSwap
//     {
//         iter_swap_calls++;
//         return std::ranges::swap(it1.s, it2.s);
//     }

//     static void reset_counters()
//     {
//         deref_calls = 0;
//         iter_move_calls = 0;
//         iter_copy_root_calls = 0;
//         iter_move_root_calls = 0;
//         iter_assign_from_calls = 0;
//         iter_swap_calls = 0;
//     }

//     static std::size_t get_deref_calls()
//     {
//         return deref_calls;
//     }

//     static std::size_t get_iter_move_calls()
//     {
//         return iter_move_calls;
//     }

//     static std::size_t get_iter_copy_root_calls()
//     {
//         return iter_copy_root_calls;
//     }

//     static std::size_t get_iter_move_root_calls()
//     {
//         return iter_move_root_calls;
//     }

//     static std::size_t get_iter_assign_from_calls()
//     {
//         return iter_assign_from_calls;
//     }

//     static std::size_t get_iter_swap_calls()
//     {
//         return iter_swap_calls;
//     }

// private:
//     S s;
//     static inline std::size_t deref_calls{};
//     static inline std::size_t iter_move_calls{};
//     static inline std::size_t iter_copy_root_calls{};
//     static inline std::size_t iter_move_root_calls{};
//     static inline std::size_t iter_assign_from_calls{};
//     static inline std::size_t iter_swap_calls{};
// };

struct S
{
    S()
    {
    }

    S(const S&)
    {
        copy_ctor_calls++;
    }

    S(S&&)
    {
        move_ctor_calls++;
    }

    static void reset_counters()
    {
        copy_ctor_calls = 0;
        move_ctor_calls = 0;
    }

    static inline std::size_t copy_ctor_calls{};
    static inline std::size_t move_ctor_calls{};
};

struct It1
{
    S operator*()
    {
        return S{};
    }
};

struct It2
{
    S& operator*()
    {
        static S s;
        return s;
    }
};

struct It3
{
    S&& operator*()
    {
        static S s;
        return std::move(s);
    }
};

void iter_move_test()
{
    {
        S::reset_counters();

        using it_t = It1;
        static_assert(!std::is_reference_v<std::iter_rvalue_reference_t<it_t>>);
        it_t it;
        auto&& d = *it;
        [[maybe_unused]] std::iter_rvalue_reference_t<it_t> r =
            stdf::iter_move(it, d);
        assert(S::copy_ctor_calls == 0);
        assert(S::move_ctor_calls == 1);
    }

    {
        S::reset_counters();

        using it_t = It2;
        static_assert(
            std::is_rvalue_reference_v<std::iter_rvalue_reference_t<it_t>>);
        it_t it;
        auto&& d = *it;
        [[maybe_unused]] std::iter_rvalue_reference_t<it_t> r =
            stdf::iter_move(it, d);
        assert(S::copy_ctor_calls == 0);
        assert(S::move_ctor_calls == 0);
    }

    {
        S::reset_counters();

        using it_t = It3;
        static_assert(
            std::is_rvalue_reference_v<std::iter_rvalue_reference_t<it_t>>);
        it_t it;
        auto&& d = *it;
        [[maybe_unused]] std::iter_rvalue_reference_t<it_t> r =
            stdf::iter_move(it, d);
        assert(S::copy_ctor_calls == 0);
        assert(S::move_ctor_calls == 0);
    }
}

// void iter_move_test()
// {
//     // test custom/default iter_move and dereferenced version. Also, return
//     // by-value/by-reference
//     // test return type of iter_move, test number of calls to move/copy.
//     // for each we need to test: that customized version is used,
//     // that value was correspondingly copied/moved only once
//     using by_ref_it_t = iterator<false>;
//     using by_val_it_t = iterator<true>;

//     static_assert(std::is_same_v<std::iter_value_t<by_ref_it_t>, int>);
//     static_assert(std::is_same_v<std::iter_reference_t<by_ref_it_t>, int&>);
//     static_assert(
//         std::is_same_v<std::iter_rvalue_reference_t<by_ref_it_t>, int&&>);
//     static_assert(std::is_same_v<
//                   stdf::iter_root_t<by_ref_it_t>,
//                   std::iter_value_t<by_ref_it_t>>);
//     static_assert(std::is_same_v<
//                   stdf::iter_root_reference_t<by_ref_it_t>,
//                   std::iter_reference_t<by_ref_it_t>>);
//     static_assert(std::is_same_v<
//                   stdf::iter_root_rvalue_reference_t<by_ref_it_t>,
//                   std::iter_rvalue_reference_t<by_ref_it_t>>);

//     static_assert(std::is_same_v<std::iter_value_t<by_val_it_t>, int>);
//     static_assert(std::is_same_v<std::iter_reference_t<by_val_it_t>, int>);
//     static_assert(
//         std::is_same_v<std::iter_rvalue_reference_t<by_val_it_t>, int>);
//     static_assert(std::is_same_v<
//                   stdf::iter_root_t<by_val_it_t>,
//                   std::iter_value_t<by_val_it_t>>);
//     static_assert(std::is_same_v<
//                   stdf::iter_root_reference_t<by_val_it_t>,
//                   std::iter_reference_t<by_val_it_t>>);
//     static_assert(std::is_same_v<
//                   stdf::iter_root_rvalue_reference_t<by_val_it_t>,
//                   std::iter_rvalue_reference_t<by_val_it_t>>);

//     by_ref_it_t by_ref_it;
// }

// TODO
// test with pure transformations which return by-value
// don't test everything, it's just a POC
int main()
{
    iter_move_test();

    projection_test();
    narrow_projection_test();
    copy_if_test();
    sort_test();
    remove_if_test();
    fill_test();
    replace_if_test();

    return 0;
}

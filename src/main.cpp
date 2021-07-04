#include <iostream>
#include <iterator>
#include <ranges>
#include <vector>
#include <algorithm>
#include <type_traits>
#include <concepts>
#include <cassert>

#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/ranges.h>
#include <fmt/ostream.h>

namespace stdf
{
template<typename T>
concept class_or_enum =
    std::is_class_v<T> || std::is_union_v<T> || std::is_enum_v<T>;

namespace iter_assign_to_cpo
{
    void iter_assign_to();

    // clang-format off
    template<typename To, typename From>
    concept has_adl_iter_assign_to = (class_or_enum<std::remove_cvref_t<To>>) &&
        requires(To&& to, From&& from)
        {
            iter_assign_to(static_cast<To&&>(to), static_cast<From&&>(from));
        };
    // clang-format on

    struct impl
    {
    private:
        template<typename To, typename From>
        static constexpr bool is_noexcept()
        {
            if constexpr(has_adl_iter_assign_to<To, From>)
            {
                return noexcept(
                    iter_assign_to(std::declval<To>(), std::declval<From>()));
            }
            else
            {
                return noexcept((*std::declval<To>()) = std::declval<From>());
            }
        }

    public:
        // clang-format off
        template<typename To, typename From>
        requires (has_adl_iter_assign_to<To, From> ||
            std::assignable_from<std::iter_reference_t<To>, From>)
        constexpr void operator()(To&& to, From&& from) const
            noexcept(is_noexcept<To, From>())
        {
            if constexpr(has_adl_iter_assign_to<To, From>)
            {
                iter_assign_to(
                    static_cast<To&&>(to), static_cast<From&&>(from));
            }
            else
            {
                *to = static_cast<From&&>(from);
            }
        }
        // clang-format on
    };
} // namespace iter_assign_to_cpo

inline namespace cpo
{
inline constexpr iter_assign_to_cpo::impl iter_assign_to{};
}

template<typename To, typename From>
concept iter_assignable_from = requires(To&& to, From&& from)
{
    stdf::iter_assign_to(static_cast<To&&>(to), static_cast<From&&>(from));
};

// namespace iter_assign_to_cpo
// {
//     void iter_assign_to();

//     // clang-format off
//     template<typename To, typename From>
//     concept has_adl_iter_assign_to = (class_or_enum<std::remove_cvref_t<To>>)
//         && requires(To&& to, From&& from)
//     {
//         iter_assign_to(static_cast<To&&>(to), static_cast<From&&>(from));
//     };
//     // clang-format on

//     struct impl
//     {
//     private:
//         template<typename To, typename From>
//         static constexpr bool is_noexcept()
//         {
//             if constexpr(has_adl_iter_assign_to<To, From>)
//             {
//                 return noexcept(
//                     iter_assign_to(std::declval<To>(),
//                     std::declval<From>()));
//             }
//             else
//             {
//                 return noexcept((*std::declval<To>()) =
//                 std::declval<From>());
//             }
//         }

//     public:
//         template<typename To, typename From>
//         constexpr void operator()(To&& to, From&& from) const
//             noexcept(is_noexcept<To, From>())
//         {
//             if constexpr(has_adl_iter_assign_to<To, From>)
//             {
//                 iter_assign_to(
//                     static_cast<To&&>(to), static_cast<From&&>(from));
//             }
//             else
//             {
//                 *to = static_cast<From&&>(from);
//             }
//         }
//     };
// } // namespace iter_assign_to_cpo

// inline namespace cpo
// {
// inline constexpr iter_assign_to_cpo::impl iter_assign_to{};
// }

namespace iter_copy_from_cpo
{
    void iter_copy_from();

    // clang-format off
template<typename From>
concept has_adl_iter_copy_from = (class_or_enum<std::remove_cvref_t<From>>)
    && requires(From&& from)
{
    // TODO: check whether it's OK to cast to rvalue
    iter_copy_from(static_cast<From&&>(from));
};
    // clang-format on

    struct impl
    {
    private:
        template<typename From>
        static constexpr bool is_noexcept()
        {
            if constexpr(has_adl_iter_copy_from<From>)
            {
                return noexcept(iter_copy_from(std::declval<From>()));
            }
            else
            {
                return noexcept(*std::declval<From>());
            }
        }

        template<typename From>
        struct result
        {
            using type = decltype(*std::declval<From>());
        };

        template<typename From>
        requires has_adl_iter_copy_from<From>
        struct result<From>
        {
            using type = decltype(iter_copy_from(std::declval<From>()));
        };

        template<typename From>
        using result_t = typename result<From>::type;

    public:
        template<typename From>
        constexpr result_t<From> operator()(From&& from) const
            noexcept(is_noexcept<From>())
        {
            if constexpr(has_adl_iter_copy_from<From>)
            {
                return iter_copy_from(static_cast<From&&>(from));
            }
            else
            {
                return *from;
            }
        }
    };
} // namespace iter_copy_from_cpo

inline namespace cpo
{
inline constexpr iter_copy_from_cpo::impl iter_copy_from{};
}

namespace iter_move_from_cpo
{
void iter_move_from();

// clang-format off
template<typename From>
concept has_adl_iter_move_from = (class_or_enum<std::remove_cvref_t<From>>)
    && requires(From&& from)
{
    iter_move_from(static_cast<From&&>(from));
};
// clang-format on

struct impl
{
private:
    template<typename From>
    static constexpr bool is_noexcept()
    {
        if constexpr(has_adl_iter_move_from<From>)
        {
            return noexcept(iter_move_from(std::declval<From>()));
        }
        else
        {
            return noexcept(std::ranges::iter_move(std::declval<From>()));
        }
    }

    template<typename From>
    struct result
    {
        using type = decltype(std::ranges::iter_move(std::declval<From>()));
    };

    template<typename From>
    requires has_adl_iter_move_from<From>
    struct result<From>
    {
        using type = decltype(iter_move_from(std::declval<From>()));
    };

    template<typename From>
    using result_t = typename result<From>::type;

public:
    template<typename From>
    constexpr result_t<From> operator()(From&& from) const
        noexcept(is_noexcept<From>())
    {
        if constexpr(has_adl_iter_move_from<From>)
        {
            return iter_move_from(static_cast<From&&>(from));
        }
        else
        {
            return std::ranges::iter_move(from);
        }
    }
};
} // namespace iter_move_from_cpo

inline namespace cpo
{
inline constexpr iter_move_from_cpo::impl iter_move_from{};
}

//------------------------------------------------------------------------------
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
} // namespace detail

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

    // ITERATOR
    //----------------------------------------------------------------

    template<bool IsConst>
    class Iterator
    {
    private:
        using ParentView = detail::maybe_const_t<IsConst, projection_view>;
        using BaseRange = detail::maybe_const_t<IsConst, Range>;

        static constexpr auto get_iter_concept()
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

        static constexpr auto get_iter_cat()
        {
            using Res = std::
                invoke_result_t<Fp&, std::ranges::range_reference_t<BaseRange>>;
            if constexpr(std::is_lvalue_reference_v<Res>)
            {
                using Cat =
                    typename std::iterator_traits<BaseIter>::iterator_category;
                if constexpr(std::derived_from<
                                 Cat,
                                 std::contiguous_iterator_tag>)
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

        using BaseIter = std::ranges::iterator_t<BaseRange>;

        BaseIter current{};
        ParentView* parent{};

        template<typename It>
        struct root_type
        {
            using type = It;
        };

        template<typename It>
        requires requires(It it)
        {
            it.root();
        }
        struct root_type<It>
        {
            using type = decltype(std::declval<It>().root());
        };

    public:
        using iterator_concept = decltype(get_iter_concept());
        using iterator_category = decltype(get_iter_cat());
        using value_type = std::remove_cvref_t<std::invoke_result_t<
            Fp&,
            std::ranges::range_reference_t<BaseRange>>>;
        using difference_type = std::ranges::range_difference_t<BaseRange>;
        using root_type_t = typename root_type<BaseIter>::type;

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
            noexcept(noexcept(std::invoke(parent->m_fun, *current)))
        {
            return std::invoke(parent->m_fun, *current);
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
            return std::invoke(parent->m_fun, current[n]);
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

        friend constexpr decltype(auto)
            iter_move(const Iterator& i) noexcept(noexcept(*i))
        {
            return std::move(*i);
        }

        friend constexpr void
            iter_swap(const Iterator& x, const Iterator& y) noexcept(
                noexcept(std::ranges::iter_swap(x.current, y.current))) requires
            std::indirectly_swappable<BaseIter>
        {
            return std::ranges::iter_swap(x.current, y.current);
        }

        // TODO: do we need const reference here?
        friend constexpr decltype(auto)
            iter_copy_from(const Iterator& it) noexcept
        {
            return iter_copy_from(it.current);
        }

        friend constexpr decltype(auto)
            iter_move_from(const Iterator& it) noexcept
        {
            return iter_move_from(it.current);
        }

        template<typename It, typename T>
        static constexpr bool is_noexcept_assignable() noexcept
        {
            if constexpr(std::assignable_from<decltype(*std::declval<It>()), T>)
            {
                return noexcept(*std::declval<It>() = std::declval<T&&>());
            }
            else
            {
                return noexcept(stdf::iter_assign_to(
                    std::declval<It>().current, std::declval<T&&>()));
            }
        }

        // XXX
        // clang-format off
        template<typename T>
        requires(
            std::assignable_from<std::iter_reference_t<BaseIter>, T> ||
            stdf::iter_assignable_from<BaseIter, T&&>)
        friend constexpr void iter_assign_to(const Iterator& it, T&& val)
            noexcept(is_noexcept_assignable<Iterator, T>())
        {
            if constexpr(std::assignable_from<decltype(*it), T>)
            {
                *it = std::forward<T>(val);
            }
            else
            {
                stdf::iter_assign_to(it.current, std::forward<T>(val));
            }
        }
        // clang-format on

        constexpr root_type_t root() const noexcept
        {
            if constexpr(requires
                         {
                             current.root();
                         })
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
    // ITERATOR
    // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    // SENTINEL
    // ----------------------------------------------------------------
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
            return m_end - i.current;
        }

        template<bool Const2>
        constexpr bool equal(const Iterator<Const2>& i) const
        {
            return i.current == m_end;
        }

        std::ranges::sentinel_t<Base> m_end{};

    public:
        Sentinel() = default;

        constexpr explicit Sentinel(std::ranges::sentinel_t<Base> end)
            : m_end(end)
        {
        }

        constexpr Sentinel(Sentinel<!IsConst> i) requires IsConst
            && std::convertible_to<
                std::ranges::sentinel_t<Range>,
                std::ranges::sentinel_t<Base>> : m_end(std::move(i.m_end))
        {
        }

        constexpr std::ranges::sentinel_t<Base> base() const
        {
            return m_end;
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
    // SENTINEL
    // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    // DATA
    // MEMBERS
    //------------------------------------------------------------
    Range m_base{};
    Fp m_fun;
    // DATA MEMBERS
    // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

public:
    // VIEW INTERFACE
    projection_view() = default;

    constexpr projection_view(Range base, Fp fun)
        : m_base{std::move(base)}, m_fun{std::move(fun)}
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

    // non-const begin()
    constexpr Iterator<false> begin()
    {
        return Iterator<false>{*this, std::ranges::begin(m_base)};
    }

    // const begin()
    constexpr Iterator<true> begin() const requires
        std::ranges::range<const Range> && std::regular_invocable<
            const Fp&,
            std::ranges::range_reference_t<const Range>>
    {
        return Iterator<true>{*this, std::ranges::begin(m_base)};
    }

    // non-const end(), returns Sentinel
    constexpr Sentinel<false> end()
    {
        return Sentinel<false>{std::ranges::end(m_base)};
    }

    // non-const common-range end(), returns Iterator
    constexpr Iterator<false> end() requires std::ranges::common_range<Range>
    {
        return Iterator<false>{*this, std::ranges::end(m_base)};
    }

    // const end(), returns Sentinel
    constexpr Sentinel<true> end() const requires
        std::ranges::range<const Range> && std::regular_invocable<
            const Fp&,
            std::ranges::range_reference_t<const Range>>
    {
        return Sentinel<true>{std::ranges::end(m_base)};
    }

    // const common-range end(), returns Iterator
    constexpr Iterator<true> end() const requires
        std::ranges::common_range<const Range> && std::regular_invocable<
            const Fp&,
            std::ranges::range_reference_t<const Range>>
    {
        return Iterator<true>{*this, std::ranges::end(m_base)};
    }

    // non-const size()
    constexpr auto size() requires std::ranges::sized_range<Range>
    {
        return std::ranges::size(m_base);
    }

    // const size()
    constexpr auto size() const requires std::ranges::sized_range<const Range>
    {
        return std::ranges::size(m_base);
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

template<std::ranges::input_range Vp, std::copy_constructible Fp>
requires std::ranges::view<Vp> && std::is_object_v<Fp> &&
    std::regular_invocable<Fp&, std::ranges::range_reference_t<Vp>> &&
    detail::can_reference<
        std::invoke_result_t<Fp&, std::ranges::range_reference_t<Vp>>>
class narrow_projection_view
    : public std::ranges::view_interface<narrow_projection_view<Vp, Fp>>
{
private:
    template<bool IsConst>
    class Sentinel;

    template<bool IsConst>
    class Iterator
    {
    private:
        using Parent = detail::maybe_const_t<IsConst, narrow_projection_view>;
        using Base = detail::maybe_const_t<IsConst, Vp>;

        static constexpr auto get_iter_concept()
        {
            if constexpr(std::ranges::random_access_range<Vp>)
            {
                return std::random_access_iterator_tag{};
            }
            else if constexpr(std::ranges::bidirectional_range<Vp>)
            {
                return std::bidirectional_iterator_tag{};
            }
            else if constexpr(std::ranges::forward_range<Vp>)
            {
                return std::forward_iterator_tag{};
            }
            else
            {
                return std::input_iterator_tag{};
            }
        }

        static constexpr auto get_iter_cat()
        {
            using Res =
                std::invoke_result_t<Fp&, std::ranges::range_reference_t<Base>>;
            if constexpr(std::is_lvalue_reference_v<Res>)
            {
                using Cat =
                    typename std::iterator_traits<BaseIter>::iterator_category;
                if constexpr(std::derived_from<
                                 Cat,
                                 std::contiguous_iterator_tag>)
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

        using BaseIter = std::ranges::iterator_t<Base>;

        BaseIter current = BaseIter();
        Parent* parent{};

    public:
        using iterator_concept = decltype(get_iter_concept());
        using iterator_category = decltype(get_iter_cat());
        using value_type = std::remove_cvref_t<
            std::invoke_result_t<Fp&, std::ranges::range_reference_t<Base>>>;
        using difference_type = std::ranges::range_difference_t<Base>;

        Iterator() = default;

        constexpr Iterator(Parent& parent, BaseIter current)
            : current(std::move(current)), parent(std::addressof(parent))
        {
        }

        constexpr Iterator(Iterator<!IsConst> i) requires IsConst
            && std::convertible_to<std::ranges::iterator_t<Vp>, BaseIter>
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
            noexcept(noexcept(std::invoke(parent->m_fun, *current)))
        {
            return std::invoke(parent->m_fun, *current);
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
            return std::invoke(parent->m_fun, current[n]);
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

        constexpr auto root() const noexcept
        {
            if constexpr(requires
                         {
                             current.root();
                         })
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
        using Base = detail::maybe_const_t<IsConst, Vp>;

        template<bool Const2>
        constexpr std::ranges::range_difference_t<Base>
            distance_from(const Iterator<Const2>& i) const
        {
            return m_end - i.current;
        }

        template<bool Const2>
        constexpr bool equal(const Iterator<Const2>& i) const
        {
            return i.current == m_end;
        }

        std::ranges::sentinel_t<Base> m_end{};

    public:
        Sentinel() = default;

        constexpr explicit Sentinel(std::ranges::sentinel_t<Base> end)
            : m_end(end)
        {
        }

        constexpr Sentinel(Sentinel<!IsConst> i) requires IsConst
            && std::convertible_to<
                std::ranges::sentinel_t<Vp>,
                std::ranges::sentinel_t<Base>> : m_end(std::move(i.m_end))
        {
        }

        constexpr std::ranges::sentinel_t<Base> base() const
        {
            return m_end;
        }

        template<bool Const2>
        requires std::sentinel_for<
            std::ranges::sentinel_t<Base>,
            std::ranges::iterator_t<detail::maybe_const_t<Const2, Vp>>>
        friend constexpr bool
            operator==(const Iterator<Const2>& x, const Sentinel& y)
        {
            return y.equal(x);
        }

        template<bool Const2>
        requires std::sized_sentinel_for<
            std::ranges::sentinel_t<Base>,
            std::ranges::iterator_t<detail::maybe_const_t<Const2, Vp>>>
        friend constexpr std::ranges::range_difference_t<Base>
            operator-(const Iterator<Const2>& x, const Sentinel& y)
        {
            return -y.distance_from(x);
        }

        template<bool Const2>
        requires std::sized_sentinel_for<
            std::ranges::sentinel_t<Base>,
            std::ranges::iterator_t<detail::maybe_const_t<Const2, Vp>>>
        friend constexpr std::ranges::range_difference_t<Base>
            operator-(const Sentinel& y, const Iterator<Const2>& x)
        {
            return y.distance_from(x);
        }

        friend Sentinel<!IsConst>;
    };

    Vp m_base{};
    Fp m_fun;

public:
    narrow_projection_view() = default;

    constexpr narrow_projection_view(Vp base, Fp fun)
        : m_base(std::move(base)), m_fun(std::move(fun))
    {
    }

    constexpr Vp base() const& requires std::copy_constructible<Vp>
    {
        return m_base;
    }

    constexpr Vp base() &&
    {
        return std::move(m_base);
    }

    constexpr Iterator<false> begin()
    {
        return Iterator<false>{*this, std::ranges::begin(m_base)};
    }

    constexpr Iterator<true> begin()
        const requires std::ranges::range<const Vp> && std::regular_invocable<
            const Fp&,
            std::ranges::range_reference_t<const Vp>>
    {
        return Iterator<true>{*this, std::ranges::begin(m_base)};
    }

    constexpr Sentinel<false> end()
    {
        return Sentinel<false>{std::ranges::end(m_base)};
    }

    constexpr Iterator<false> end() requires std::ranges::common_range<Vp>
    {
        return Iterator<false>{*this, std::ranges::end(m_base)};
    }

    constexpr Sentinel<true> end()
        const requires std::ranges::range<const Vp> && std::regular_invocable<
            const Fp&,
            std::ranges::range_reference_t<const Vp>>
    {
        return Sentinel<true>{std::ranges::end(m_base)};
    }

    constexpr Iterator<true> end() const requires
        std::ranges::common_range<const Vp> && std::regular_invocable<
            const Fp&,
            std::ranges::range_reference_t<const Vp>>
    {
        return Iterator<true>{*this, std::ranges::end(m_base)};
    }

    constexpr auto size() requires std::ranges::sized_range<Vp>
    {
        return std::ranges::size(m_base);
    }

    constexpr auto size() const requires std::ranges::sized_range<const Vp>
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

template<typename Out>
constexpr Out copy_if(auto&& in, Out out, auto pred)
{
    auto first = std::ranges::begin(in);
    auto last = std::ranges::end(in);
    for(; first != last; ++first)
    {
        auto&& x = *first;
        if(std::invoke(pred, x))
        {
            if constexpr(iter_copy_from_cpo::has_adl_iter_copy_from<
                             decltype(first)>)
            {
                stdf::iter_assign_to(out, iter_copy_from(first));
            }
            else
            {
                stdf::iter_assign_to(out, std::forward<decltype(x)>(x));
            }

            ++out;
        }
    }

    //     return out;
    // }

    // simple selection sort
    template<
        std::ranges::random_access_range R,
        typename Cmp = std::ranges::less>
    void sort(R && r, Cmp cmp = {})
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

    template<std::ranges::forward_range R, typename Pred>
    constexpr std::ranges::borrowed_subrange_t<R> remove_if(R && r, Pred pred)
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
                    if constexpr(iter_move_from_cpo::has_adl_iter_move_from<
                                     decltype(first)>)
                    {
                        iter_assign_to(first, iter_move_from(i));
                    }
                    else
                    {
                        iter_assign_to(first, std::forward<decltype(x)>(x));
                    }

                    ++first;
                }
            }
            return {first, i};
        }

        return {first, first};
    }

    // originally fill doesn't support projections at all
    template<class T, std::ranges::output_range<const T&> R>
    constexpr std::ranges::borrowed_iterator_t<R> fill(R && r, const T& value)
    {
        auto first = std::ranges::begin(r);
        auto last = std::ranges::end(r);
        for(; first != last; ++first)
        {
            iter_assign_to(first, value);
        }
        return first;
    }
} // namespace stdf

struct Y
{
    int a;
    int b;

    auto operator<=>(const Y&) const = default;

    friend std::ostream& operator<<(std::ostream& os, const Y& v)
    {
        return os << "(" << v.a << ", " << v.b << ")";
    }
};

struct X
{
    int x;
    Y y;

    auto operator<=>(const X&) const = default;

    friend std::ostream& operator<<(std::ostream& os, const X& v)
    {
        return os << "(" << v.x << ", " << v.y << ")";
    }
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

    auto copied = stdf::iter_copy_from(it1);
    auto moved = stdf::iter_move_from(it2);
    static_assert(std::is_same_v<decltype(stdf::iter_copy_from(it1)), Y&>);
    static_assert(std::is_same_v<decltype(stdf::iter_move_from(it2)), Y&&>);
    assert((copied == Y{2, 20}));
    assert((moved == Y{1, 10}));

    stdf::iter_assign_to(it1, moved);
    stdf::iter_assign_to(it2, copied);
    assert((v[0] == Y{1, 10}));
    assert((v[1] == Y{2, 20}));

    stdf::iter_assign_to(it1, copied);
    assert((v[0] == Y{2, 20}));
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

    auto copied = stdf::iter_copy_from(it1);
    auto moved = stdf::iter_move_from(it2);
    static_assert(std::is_same_v<decltype(stdf::iter_copy_from(it1)), int&>);
    static_assert(std::is_same_v<decltype(stdf::iter_move_from(it2)), int&&>);
    assert(copied == 2);
    assert(moved == 1);

    stdf::iter_assign_to(it1, moved);
    stdf::iter_assign_to(it2, copied);
    assert((v[0] == Y{1, 10}));
    assert((v[1] == Y{2, 20}));

    stdf::iter_assign_to(it1, copied);
    assert((v[0] == Y{2, 10}));
}

void print(auto&& coll)
{
    for(const auto& item : coll)
    {
        std::cout << item << ", ";
    }
    std::cout << '\n';
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
    // auto pv = v | std::views::take(1) | projection(&X::y) |
    // projection(&Y::a); auto removed = stdf::remove_if(pv,
    // less_than<int{30}>{});

    // next problem, the returned range is actually a range of two
    // `projected_view<...>::Iterator`s and `std::vector::erase()` expects
    // true vector iterators.
    // Does it make sense for sentinel?
    v.erase(removed.begin().root(), removed.end().root());
    assert((v == std::vector<X>{{3, {30, 300}}}));

    auto removed2 = stdf::remove_if(v2, less_than<X{3, {30, 300}}>{});
    v2.erase(removed2.begin(), removed2.end());
    assert((v2 == std::vector<X>{{3, {30, 300}}}));
}

template<typename T>
T tt(T&&);

void fill_test()
{
    using namespace stdf::views;

    std::vector<X> v{{1, {10, 100}}, {2, {20, 200}}, {3, {30, 300}}};
    auto pv = v | projection(&X::x);

    // auto b = std::ranges::begin(pv);
    // using bt = decltype(b);
    // static_assert(stdf::iter_assign_to_cpo::has_adl_iter_assign_to<bt, int>);
    // bt::test_assign_to(b, 1);
    // bt::test_assign_to(b, X{});

    stdf::fill(pv, 5);
    assert(
        (v == std::vector<X>{{5, {10, 100}}, {5, {20, 200}}, {5, {30, 300}}}));

    stdf::fill(v, X{1, {10, 100}});
    assert(
        (v == std::vector<X>{{1, {10, 100}}, {1, {10, 100}}, {1, {10, 100}}}));
}

void iter_assign_to_test()
{
    using namespace stdf::views;

    std::vector<X> v{{1, {10, 100}}, {2, {20, 200}}, {3, {30, 300}}};
    auto pv = v | projection(&X::x);

    auto b = std::ranges::begin(pv);
    using bt = decltype(b);
    // static_assert(stdf::iter_assign_to_cpo::has_adl_iter_assign_to2<bt,
    // int>);

    // static_assert(requires
    //               {
    //                   stdf::iter_assign_to(b, 1);
    //               });
    // static_assert(stdf::iter_assign_to_cpo::has_adl_iter_assign_to2<bt, X>);
    // *b = 1;
    stdf::iter_assign_to(b, 1);
    stdf::iter_assign_to(b, stdf::iter_copy_from(b));

    static_assert(stdf::iter_assignable_from<bt, int>);
    static_assert(stdf::iter_assignable_from<bt, X>);
    static_assert(!stdf::iter_assignable_from<bt, Y>);
    // stdf::iter_assign_to(b, Y{});
}

// TODO
// specify noexcept-ness, looks like Iterator's and CPO share the common
// is_noexcept() functionality, I think we need to combine it
// rename iter_assign_to() to iter_assign_from()
// better algo implementations
// move `get_iter_category/concept()` to the `detail` namespace
// check concepts, I guess that iter_assing_to() must be sfinae friendly.
// yes, it will be required for indirectly_writable.
// update/add concepts and use them to constrain provided algorithms
// Can we eliminate iter_assign_to() if there's not adl version?
// this can save yet another operator*() call.
int main()
{
    projection_test();
    narrow_projection_test();
    copy_if_test();
    sort_test();
    remove_if_test();
    fill_test();
    iter_assign_to_test();

    return 0;
}